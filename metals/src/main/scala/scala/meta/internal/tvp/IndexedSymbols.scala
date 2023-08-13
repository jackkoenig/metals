package scala.meta.internal.tvp

import scala.collection.concurrent.TrieMap

import scala.meta.Dialect
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.mtags.GlobalSymbolIndex
import scala.meta.internal.mtags.Symbol
import scala.meta.internal.mtags.SymbolDefinition
import scala.meta.internal.semanticdb.SymbolInformation
import scala.meta.io.AbsolutePath

class IndexedSymbols(index: GlobalSymbolIndex) { // , isStatisticsEnabled: Boolean

  // TODO maybe cache pure TreeViewNode
  // Used for workspace, is eager
  private val workspaceCache = TrieMap.empty[
    AbsolutePath,
    Array[SymbolDefinition],
  ]

  // Used for dependencies, is lazy
  // TODO maybe use Either?
  private val jarCache = TrieMap.empty[
    AbsolutePath,
    TrieMap[String, (SymbolDefinition, Array[SymbolDefinition])],
  ]

  def clearCache(path: AbsolutePath): Unit = {
    jarCache.remove(path)
    workspaceCache.remove(path)
  }

  def reset(): Unit = {
    jarCache.clear()
    workspaceCache.clear()
  }

  /**
   * We load all symbols for workspace eagerly
   *
   * @param in
   * @param dialect
   * @return
   */
  def workspaceSymbols(
      in: AbsolutePath,
      symbol: String,
      dialect: Dialect,
  ): Iterator[TreeViewSymbolInformation] = {

    val syms = workspaceCache
      .getOrElseUpdate(
        in,
        members(in, dialect),
      )
    if (Symbol(symbol).isRootPackage) syms.map(toTreeView).iterator
    else
      syms.collect {
        case defn if defn.definitionSymbol.value.startsWith(symbol) =>
          toTreeView(defn)
      }.iterator
  }

  // TODO JAva seems to not be loaded at all since toplevels are not saved
  def jarSymbols(
      in: AbsolutePath,
      symbol: String,
      dialect: Dialect,
  ): Iterator[TreeViewSymbolInformation] = {
    val realIn =
      if (in.isJar && in.filename.contains("-sources.jar")) in
      else in.parent.resolve(in.filename.replace(".jar", "-sources.jar"))

    val syms = jarCache.getOrElseUpdate(
      realIn, {
        val toplevels = index
          .toplevelsAt(in, dialect)
          .map(defn =>
            defn.definitionSymbol.value -> (defn, Array.empty[SymbolDefinition])
          )

        TrieMap.empty[
          String,
          (SymbolDefinition, Array[SymbolDefinition]),
        ] ++ toplevels
      },
    )
    def toplevelOwner(symbol: Symbol): Symbol = {
      if (syms.contains(symbol.value)) symbol
      else toplevelOwner(symbol.owner)
    }
    syms.get(toplevelOwner(Symbol(symbol)).value) match {
      case Some((defn, Array())) =>
        val children = members(defn.path, dialect)
        syms.put(symbol, (defn, children))
        children.map(toTreeView).iterator
      case Some((_, calculated)) =>
        calculated.map(toTreeView).iterator
      case _ => Iterator.empty[TreeViewSymbolInformation]
    }

  }

  private def members(
      path: AbsolutePath,
      dialect: Dialect,
  ): Array[SymbolDefinition] = {
    index.symbolsAt(path, dialect).toArray
  }

  private def toTreeView(
      symDef: SymbolDefinition
  ): TreeViewSymbolInformation = {
    val kind = symDef.kind match {
      case Some(SymbolInformation.Kind.UNKNOWN_KIND) | None =>
        if (symDef.definitionSymbol.isMethod) SymbolInformation.Kind.METHOD
        else if (symDef.definitionSymbol.isType) SymbolInformation.Kind.CLASS
        else if (symDef.definitionSymbol.isTypeParameter)
          SymbolInformation.Kind.TYPE_PARAMETER
        else SymbolInformation.Kind.OBJECT
      case Some(knownKind) => knownKind
    }
    TreeViewSymbolInformation(
      symDef.definitionSymbol.value,
      kind,
      0,
    )
  }

}
