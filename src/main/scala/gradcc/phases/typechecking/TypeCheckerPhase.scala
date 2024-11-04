package gradcc.phases.typechecking

import gradcc.*
import gradcc.asts.{TypedTermTree, UniqueVarId, TypedTerms as T, UniquelyNamedTerms as U}
import gradcc.lang.*
import gradcc.phases.SimplePhase
import gradcc.phases.prettyprinting.TermsPrettyprinter
import gradcc.phases.typechecking.SubtypingRelation.*
import gradcc.reporting.{Position, Reporter}

// TODO check pack with Alex
// TODO double-check every typing/subtyping/subcapturing rule

final class TypeCheckerPhase extends SimplePhase[U.TermTree, TypedTermTree[T.TermTree]]("Typechecker") {
  private val varCreator = SyntheticVarCreator()

  private val pp: TypedTermTree[T.TermTree] => String = TermsPrettyprinter(T)

  override val acceptsFaultyInput: Boolean = false

  override protected def runImpl(in: U.TermTree, reporter: Reporter): TypedTermTree[T.TermTree] =
    typeTerm(in)(using Ctx(reporter))

  private def typeTerm(t: U.TermTree)(using ctx: Ctx): TypedTermTree[T.TermTree] = t match {
    case p: U.StablePathTree => typeStablePath(p)
    case U.CapTree(position) =>
      throw AssertionError("unexpected type computation on the root capability")
    case abs@U.AbsTree(varIdent, varTypeTree, body, position) => {
      val varType = U.mkType(varTypeTree)
      val varId = varIdent.id
      val typedBody = typeTerm(body)(using ctx.withNewBinding(varId, Some(varType)))
      T.AbsTree(convertIdent(varIdent), typeTypeTree(varTypeTree), typedBody, position).withType(
        typedBody.tpe.map(AbsShape(varId, varType, _) ^ cv(abs))
      )
    }
    case U.BoxTree(boxed, position) =>
      val typedBoxed = typeStablePath(boxed)
      T.BoxTree(typedBoxed, position).withType(
        typedBoxed.tpe.map(BoxShape(_) ^ CaptureSet.empty)
      )
    case recordLit@U.RecordLiteralTree(fields, position) =>
      val typedFields = convertFields(fields, typeStablePath)
      val fieldsToTypesAndCapSetOpt = collectTypes(typedFields).map { fldTypes =>
        val fieldsToType = fields.map((fld, _) => U.mkField(fld)).zip(fldTypes).toMap
        val recordCapSet = fldTypes.foldLeft[CaptureDescriptor](CaptureSet(Set.empty)){ (accCSet, tpe) =>
          accCSet ++ tpe.captureDescr
        }
        (fieldsToType, recordCapSet)
      }
      val selfReferringTypeOpt = fieldsToTypesAndCapSetOpt.map { (rawFieldsToTypes, rawCapSet) =>
        val freePathsInFields = rawFieldsToTypes.flatMap((_, tpe) => freePaths(tpe))
        val selfRef = varCreator.nextVar(Keyword.SelfKw.str)
        val selfAwareCtx = ctxWithEquivalences(ctx, selfRef, recordLit)
        val selfRefPath = VarPath(selfRef)
        val substMap: Map[StablePath, StablePath] =
          (for (freeP <- freePathsInFields; pathFromSelf <- selfAwareCtx.expressAsPathFrom(selfRefPath, freeP)) yield {
            freeP -> pathFromSelf
          }).toMap
        val substFieldsToTypes = rawFieldsToTypes.map((fld, tpe) => (fld, substitute(tpe)(using substMap)))
        val filteredCapSet = rawCapSet.removed(substMap.keys)
        val optSelfRef = if substMap.isEmpty then None else Some(selfRef)
        RecordShape(optSelfRef, substFieldsToTypes) ^ filteredCapSet
      }
      val letTypeOpt = selfReferringTypeOpt.orElse {
        fieldsToTypesAndCapSetOpt.map { (fieldsToTypes, capSet) =>
          RecordShape(None, fieldsToTypes) ^ capSet
        }
      }
      T.RecordLiteralTree(typedFields, position).withType(letTypeOpt)
    case U.UnitLiteralTree(position) =>
      T.UnitLiteralTree(position).withType(UnitShape ^ CaptureSet.empty)
    case U.AppTree(callee, arg, position) => {
      val typedCallee = typeStablePath(callee)
      val typedArg = typeStablePath(arg)
      val tpeOpt = (typedCallee.tpe, typedArg.tpe) match {
        case (None, _) => None
        case (_, None) => None
        case (Some(Type(AbsShape(varId, varType, resType), capturedByAbs)), Some(argType)) =>
          mustBeAssignable(varType, argType, arg.position, {
            Some(substitute(resType)(using Map(VarPath(varId) -> U.mkStablePath(arg))))
          })
        case (Some(callerType), _) =>
          ctx.reportError(s"$callerType is not callable", position)
      }
      T.AppTree(typedCallee, typedArg, position).withType(tpeOpt)
    }
    case U.UnboxTree(captureDescr, boxed, position) => {
      if (captureDescr.isInstanceOf[U.BrandDescriptorTree]){
        ctx.gradualityUsed(position)
      }
      val typedCapDescr = typeCaptureDescr(captureDescr)
      val typedBoxed = typeStablePath(boxed)
      val unboxCapSet = T.mkCaptureDescr(typedCapDescr)
      T.UnboxTree(typedCapDescr, typedBoxed, position).withType(
        typedBoxed.tpe.flatMap {
          case Type(BoxShape(boxed), _) if boxed.captureDescr == unboxCapSet => Some(boxed)
          case Type(BoxShape(boxed), _) =>
            ctx.reportError(s"illegal unboxing: the capture set ${boxed.captureDescr} of the unboxed type " +
              s"differs from the capture set $unboxCapSet mentioned by the unbox term", position)
          case tpe => ctx.reportError(s"cannot unbox non-box type $tpe", position)
        }
      )
    }
    case U.LetTree(varId, value, typeAnnot, body, position) => {
      val typedValue = typeTerm(value)
      for (rawValueType <- typedValue.tpe; typeAnnot <- typeAnnot) {
        mustBeAssignable(U.mkType(typeAnnot), rawValueType, typeAnnot.position, None)
      }
      val valueType = typeAnnot.map(U.mkType).orElse(typedValue.tpe)
      val bodyCtx = ctxWithEquivalences(ctx, varId.id, value).withNewBinding(varId.id, valueType)
      val typedBody = typeTerm(body)(using bodyCtx)
      val letTypeOpt =
        typedBody.tpe.flatMap { bodyType =>
          if (varId.id.isFreeIn(bodyType)) {
            bodyCtx.equivalenceClassOf(varId.id).filterNot(_ == varId.id).headOption.map { replId =>
              substitute(bodyType)(using Map(VarPath(varId.id) -> VarPath(replId)))
            }.orElse(
              ctx.reportError(
                s"forbidden capture: let body has type $bodyType, which depends on let-bound variable ${varId.id}",
                position
              ))
          } else Some(bodyType)
        }
      T.LetTree(convertIdent(varId), typedValue, typeAnnot.map(typeTypeTree), typedBody, position).withType(letTypeOpt)
    }
    case U.RegionTree(position) =>
      T.RegionTree(position).withType(RegionShape ^ CaptureSet(RootCapability))
    case U.DerefTree(ref, position) =>
      val typedRef = typeStablePath(ref)
      T.DerefTree(typedRef, position).withType(
        typedRef.tpe.flatMap {
          case Type(RefShape(referenced), captureSet) => Some(referenced ^ CaptureSet.empty)
          case tpe => ctx.reportError(s"illegal dereference: $tpe is not a reference", position)
        }
      )
    case U.AssignTree(ref, newVal, position) => {
      val typedRef = typeStablePath(ref)
      val typedNewVal = typeStablePath(newVal)
      (typedRef.tpe, typedNewVal.tpe) match {
        case (Some(Type(RefShape(referenced), _)), Some(valType)) => {
          // TODO check that we indeed need to box branded types
          if valType.captureDescr.isCapSetOfPureType
          then mustBeAssignable(referenced, valType.shape, position, Some(UnitShape ^ CaptureSet.empty))
          else ctx.reportError(
            s"illegal assignment: capture set ${valType.captureDescr} of assigned value is not empty, please fix this by boxing it",
            position
          )
        }
        case (Some(nonRefType), Some(_)) =>
          ctx.reportError(s"expected a reference type as assignment target, found $nonRefType", position)
        case _ => ()
      }
      T.AssignTree(typedRef, typedNewVal, position).withType(UnitShape ^ CaptureSet.empty)
    }
    case U.RefTree(regionCap, initVal, position) =>
      val typedRegionCap = typeStablePath(regionCap)
      val typedInitVal = typeStablePath(initVal)
      val tpeOpt = (typedRegionCap.tpe, typedInitVal.tpe) match {
        case (Some(Type(RegionShape, _)), Some(Type(initValShape, initValCaptureSet))) =>
          // TODO check that we indeed need to box branded types
          if initValCaptureSet.isCapSetOfPureType
          then Some(RefShape(initValShape) ^ capDescrFor(U.mkStablePath(regionCap)))
          else ctx.reportError(
            "value assigned to reference must have an empty capture set, please fix that by boxing it",
            position)
        case (Some(nonRegionType), _) =>
          ctx.reportError(s"expected a region, found $nonRegionType", position)
        case _ => None
      }
      T.RefTree(typedRegionCap, typedInitVal, position).withType(tpeOpt)
    case U.ModuleTree(regionCap, fields, position) => {
      val typedRegionCap = typeStablePath(regionCap)
      typedRegionCap.tpe.foreach { regionCapType =>
        mustBeAssignable(RegionShape ^ CaptureSet(RootCapability), regionCapType, regionCap.position, None)
      }
      val selfRefVar = varCreator.nextVar(Keyword.SelfKw.str)
      val regionCapPath = U.mkStablePath(regionCap)
      val substFields = fields.map(
        (fld, p) =>
          val regPath = SelectPath(VarPath(selfRefVar), RegionField)
          val TypedTermTree(convP, pType) = typeStablePath(p)
          convertField(fld) -> TypedTermTree(convP, pType.map(substitute(_)(using Map(regionCapPath -> regPath))))
      )
      val tpeOpt = collectTypes(substFields).map { fieldTypes =>
        val fieldsToTypes = substFields.map((fld, _) => T.mkField(fld)).zip(fieldTypes).toMap
        val regType = RegionShape ^ CaptureSet(RootCapability)
        RecordShape(Some(selfRefVar), fieldsToTypes.updated(RegionField, regType)) ^ CaptureSet(RootCapability)
      }
      T.ModuleTree(typedRegionCap, substFields, position).withType(tpeOpt)
    }
    case U.EnclosureTree(permissions, explicitTypeTree, body, position) => {
      val typedPermissions = typeCaptureSet(permissions)
      val typedExplicitType = typeTypeTree(explicitTypeTree)
      val typedBody = typeTerm(body)(using ctx.withEnclosureFlag)
      val explicitType = U.mkType(explicitTypeTree)
      typedBody.tpe.foreach { bodyType =>
        mustBeAssignable(explicitType, bodyType, position, {
          Some(explicitType)
        })
      }
      T.EnclosureTree(typedPermissions, typedExplicitType, typedBody, position).withType(explicitType)
    }
    case U.ObscurTree(obscured, varId, body, position) => {
      ctx.gradualityUsed(position)
      val typedObscured = typeStablePath(obscured)
      val convertedVarId = convertIdent(varId)
      val obscuredTypeCapturingRoot = typedObscured.tpe.map(_.copy(captureDescr = CaptureSet(RootCapability)))
      val newCtx = ctx.withNewBinding(varId.id, obscuredTypeCapturingRoot)
      val typedBody = typeTerm(body)(using newCtx)
      T.ObscurTree(typedObscured, convertedVarId, typedBody, position).withType(typedBody.tpe)
    }
  }

  private def typeStablePath(p: U.StablePathTree)(using ctx: Ctx): TypedTermTree[T.StablePathTree] = p match {
    case properPathTree: U.ProperPathTree => typeProperPath(properPathTree)
    case U.BrandedPathTree(properPath, position) =>
      ctx.gradualityUsed(p.position)
      val typedProperPath = typeProperPath(properPath)
      T.BrandedPathTree(typedProperPath, position).withType(
        typedProperPath.tpe.map(_.copy(captureDescr = Brand))
      )
  }

  private def typeProperPath(p: U.ProperPathTree)(using ctx: Ctx): TypedTermTree[T.ProperPathTree] = p match {
    case U.IdentifierTree(id, position) =>
      // id must be found, o.w. the renaming phase stops the pipeline before it reaches this point
      T.IdentifierTree(id, position).withType(ctx.varLookup(id))
    case U.SelectTree(lhs, field, position) =>
      val typedLhs = typeProperPath(lhs)
      val fld = U.mkField(field)
      T.SelectTree(typedLhs, convertField(field), position).withType(
        typedLhs.tpe
          .map(unpackIfRecursive(_, U.mkProperPath(lhs)))
          .flatMap {
            case Type(RecordShape(selfRef, fields), captureSet) if fields.contains(fld) =>
              Some(fields.apply(fld))
            case otherType =>
              ctx.reportError(s"no '$fld' field found in owner type $otherType", position)
          }
      )
  }

  private def convertIdent(ident: U.IdentifierTree)(using Ctx): T.IdentifierTree = {
    val U.IdentifierTree(id, position) = ident
    T.IdentifierTree(id, position)
  }

  private def typeTypeTree(tt: U.TypeTree)(using Ctx): T.TypeTree = {
    val U.TypeTree(shape, captureSet, position) = tt
    T.TypeTree(typeShapeTree(shape), captureSet.map(typeCaptureDescr), position)
  }

  private def typeShapeTree(shape: U.ShapeTree)(using Ctx): T.ShapeTree = shape match {
    case U.TopShapeTree(position) =>
      T.TopShapeTree(position)
    case U.AbsShapeTree(varId, varType, resType, position) =>
      T.AbsShapeTree(convertIdent(varId), typeTypeTree(varType), typeTypeTree(resType), position)
    case U.BoxShapeTree(boxedType, position) =>
      T.BoxShapeTree(typeTypeTree(boxedType), position)
    case U.UnitShapeTree(position) =>
      T.UnitShapeTree(position)
    case U.RefShapeTree(referencedType, position) =>
      T.RefShapeTree(typeShapeTree(referencedType), position)
    case U.RegShapeTree(position) =>
      T.RegShapeTree(position)
    case U.RecordShapeTree(selfRef, fieldsInOrder, position) =>
      T.RecordShapeTree(
        selfRef.map(convertIdent),
        convertFields(fieldsInOrder, typeTypeTree),
        position
      )
  }

  private def typeCaptureDescr(capDescr: U.CaptureDescriptorTree)(using ctx: Ctx): T.CaptureDescriptorTree = capDescr match {
    case capSet: U.CaptureSetTree => typeCaptureSet(capSet)
    case U.BrandDescriptorTree(position) => T.BrandDescriptorTree(position)
  }

  private def typeCaptureSet(capSet: U.CaptureSetTree)(using ctx: Ctx): T.CaptureSetTree = capSet match {
    case U.NonRootCaptureSetTree(capturedVarsInOrder, position) =>
      val typedCapturedVars = capturedVarsInOrder.map(typeProperPath)
      typedCapturedVars.filter(_.tpe.exists(_.isPure)).foreach { capVar =>
        ctx.reporter.warning(s"path ${pp(capVar)} has an empty capture set and is thus not a capability",
          capVar.term.position)
      }
      T.NonRootCaptureSetTree(typedCapturedVars, position)
    case U.RootCaptureSetTree(position) =>
      T.RootCaptureSetTree(position)
  }

  private def convertField(fld: U.FieldTree)(using Ctx): T.FieldTree = fld match {
    case U.NamedFieldTree(fieldName, position) =>
      T.NamedFieldTree(fieldName, position)
    case U.RegFieldTree(position) =>
      T.RegFieldTree(position)
  }

  private def convertFields[A, B](fields: Seq[(U.FieldTree, A)], typingFunc: A => B)(using Ctx): Seq[(T.FieldTree, B)] =
    fields.map((fld, a) => (convertField(fld), typingFunc(a)))

  private def capDescrFor(stablePath: StablePath): CaptureDescriptor = stablePath match {
    case p: ProperPath => CaptureSet(p)
    case BrandedPath(p) => Brand
  }

  private def mustBeAssignable(expectedType: Type, actualType: Type, pos: Position, ifAssignable: => Option[Type])
                              (using ctx: Ctx): Option[Type] = {
    val isSub = actualType.subtypeOf(expectedType)
    if (isSub) {
      ifAssignable
    } else {
      ctx.reportError(s"type mismatch: expected $expectedType, but was $actualType", pos)
    }
  }

  private def mustBeAssignable(expectedShape: Shape, actualShape: Shape, pos: Position, ifAssignable: => Option[Type])
                              (using ctx: Ctx): Option[Type] = {
    val isSub = actualShape.subshapeOf(expectedShape)
    if (isSub) {
      ifAssignable
    } else {
      ctx.reportError(s"type mismatch: expected $expectedShape, but was $actualShape", pos)
    }
  }

  private def ctxWithEquivalences(initialCtx: Ctx, varId: UniqueVarId, value: U.TermTree): Ctx = {
    // TODO maybe record equivalences on branded paths too?
    value match {
      case pathTree: U.ProperPathTree =>
        initialCtx.withNewPathEquivalence(VarPath(varId), U.mkProperPath(pathTree))
      case recordLiteralTree: U.RecordLiteralTree =>
        val varPath = VarPath(varId)
        recordLiteralTree.fields.foldLeft(initialCtx) {
          case (ctx, (fld, fldVal: U.ProperPathTree)) =>
            ctx.withNewSelectEquivalence(varPath, U.mkField(fld), U.mkProperPath(fldVal))
          case (ctx, (fld, fldVal)) => ctx
        }
      case _ => initialCtx
    }
  }

  extension [A <: T.TermTree](term: A) {
    private def withType(tpe: Option[Type]): TypedTermTree[A] = TypedTermTree(term, tpe)
    private def withType(tpe: Type): TypedTermTree[A] = term.withType(Some(tpe))
  }

  private def typeDescr(optType: Option[Type]): String =
    optType.map(_.toString).getOrElse("??")

  private def collectTypes[B <: T.TermTree](seq: Seq[(T.FieldTree, TypedTermTree[B])]): Option[Seq[Type]] = {
    seq.foldRight(Option(Nil)) {
      case ((_, TypedTermTree(_, tpe)), acc) => for tpe <- tpe; tail <- acc yield tpe :: tail
    }
  }

}
