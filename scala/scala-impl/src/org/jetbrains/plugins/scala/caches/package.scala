package org.jetbrains.plugins.scala

import com.intellij.openapi.util.ModificationTracker
import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.caches.CacheInUserData._
import org.jetbrains.plugins.scala.caches.CacheWithRecursionGuard.{cacheWithRecursionGuard0, cacheWithRecursionGuardN}
import org.jetbrains.plugins.scala.caches.stats.Tracer

package object caches {

  // TODO Detect control flow exceptions

  // The helpers are `inline` so that `CacheIds` computes the cache id and display name at compile time, from the call site.

  inline def cached[R](inline name: String, modificationTracker: => ModificationTracker, f: () => R): () => R =
    cached0(CacheIds.id(name), CacheIds.display(name), modificationTracker, f)

  inline def cached[T1, R](inline name: String, modificationTracker: => ModificationTracker, f: T1 => R): T1 => R =
    cached0(CacheIds.id(name), CacheIds.display(name), modificationTracker, f)

  inline def cached[T1, T2, R](inline name: String, modificationTracker: => ModificationTracker, f: (T1, T2) => R): (T1, T2) => R =
    cached0(CacheIds.id(name), CacheIds.display(name), modificationTracker, f)

  inline def cached[T1, T2, T3, R](inline name: String, modificationTracker: => ModificationTracker, f: (T1, T2, T3) => R): (T1, T2, T3) => R =
    cached0(CacheIds.id(name), CacheIds.display(name), modificationTracker, f)

  inline def cached[T1, T2, T3, T4, R](inline name: String, modificationTracker: => ModificationTracker, f: (T1, T2, T3, T4) => R): (T1, T2, T3, T4) => R =
    cached0(CacheIds.id(name), CacheIds.display(name), modificationTracker, f)

  inline def cachedWithoutModificationCount[R](inline name: String, wrapper: ValueWrapper[R], cleanupScheduler: CleanupScheduler, f: () => R): () => R =
    cachedWithoutModificationCount0(CacheIds.id(name), CacheIds.display(name), wrapper, cleanupScheduler, f)

  inline def cachedWithoutModificationCount[T1, R](inline name: String, wrapper: ValueWrapper[R], cleanupScheduler: CleanupScheduler, f: T1 => R): T1 => R =
    cachedWithoutModificationCount0(CacheIds.id(name), CacheIds.display(name), wrapper, cleanupScheduler, f)

  inline def cachedWithoutModificationCount[T1, T2, R](inline name: String, wrapper: ValueWrapper[R], cleanupScheduler: CleanupScheduler, f: (T1, T2) => R): (T1, T2) => R =
    cachedWithoutModificationCount0(CacheIds.id(name), CacheIds.display(name), wrapper, cleanupScheduler, f)

  // TODO Factory method instead of the ProjectUserDataHolder type class

  inline def cachedInUserData[E: ProjectUserDataHolder, R](inline name: String, dataHolder: E, dependency: => AnyRef)(f: => R): R =
    cacheInUserData0(CacheIds.id(name), CacheIds.display(name), dataHolder, dependency, f)

  inline def cachedInUserData[E: ProjectUserDataHolder, T <: Product, R](inline name: String, dataHolder: E, dependency: => AnyRef, v: T)(f: => R): R =
    cacheInUserDataN[E, T, R](CacheIds.id(name), CacheIds.display(name), dataHolder, dependency, v, f)

  // TODO (defaultValue: => R) parameter list

  inline def cachedWithRecursionGuard[R](inline name: String, element: PsiElement, defaultValue: => R, dependency: => AnyRef)(f: => R): R =
    cacheWithRecursionGuard0(CacheIds.id(name), CacheIds.display(name), element, defaultValue, dependency, f)

  inline def cachedWithRecursionGuard[T <: Product, R](inline name: String, element: PsiElement, defaultValue: => R, dependency: => AnyRef, v: T)(f: => R): R =
    cacheWithRecursionGuardN[T, R](CacheIds.id(name), CacheIds.display(name), element, defaultValue, dependency, v, f)

  inline def measure[R](inline name: String)(f: => R): R =
    measure0(CacheIds.id(name), CacheIds.display(name), f)

  private def measure0[R](id: String, name: String, f: => R): R = {
    val tracer = Tracer(id, name)
    tracer.invocation()
    tracer.calculationStart()
    try {
      f
    } finally {
      tracer.calculationEnd()
    }
  }

  private def cached0[R](id: String, name: String, modificationTracker: => ModificationTracker, f: () => R): () => R = {
    val cache = new Cache0[R](id, name, modificationTracker)
    () => cache { f() }
  }

  private def cached0[T1, R](id: String, name: String, modificationTracker: => ModificationTracker, f: T1 => R): T1 => R = {
    val cache = new CacheN[Tuple1[T1], R](id, name, modificationTracker)
    v1 => cache(Tuple1(v1)) { f(v1) }
  }

  private def cached0[T1, T2, R](id: String, name: String, modificationTracker: => ModificationTracker, f: (T1, T2) => R): (T1, T2) => R = {
    val cache = new CacheN[(T1, T2), R](id, name, modificationTracker)
    (v1, v2) => cache((v1, v2)) { f(v1, v2) }
  }

  private def cached0[T1, T2, T3, R](id: String, name: String, modificationTracker: => ModificationTracker, f: (T1, T2, T3) => R): (T1, T2, T3) => R = {
    val cache = new CacheN[(T1, T2, T3), R](id, name, modificationTracker)
    (v1, v2, v3) => cache((v1, v2, v3)) { f(v1, v2, v3) }
  }

  private def cached0[T1, T2, T3, T4, R](id: String, name: String, modificationTracker: => ModificationTracker, f: (T1, T2, T3, T4) => R): (T1, T2, T3, T4) => R = {
    val cache = new CacheN[(T1, T2, T3, T4), R](id, name, modificationTracker)
    (v1, v2, v3, v4) => cache((v1, v2, v3, v4)) { f(v1, v2, v3, v4) }
  }

  private def cachedWithoutModificationCount0[R](id: String, name: String, wrapper: ValueWrapper[R], cleanupScheduler: CleanupScheduler, f: () => R): () => R = {
    val cache = new CacheWithoutModificationCount0[R](id, name, wrapper, cleanupScheduler)
    () => cache { f() }
  }

  private def cachedWithoutModificationCount0[T1, R](id: String, name: String, wrapper: ValueWrapper[R], cleanupScheduler: CleanupScheduler, f: T1 => R): T1 => R = {
    val cache = new CacheWithoutModificationCountN[Tuple1[T1], R](id, name, wrapper, cleanupScheduler)
    v1 => cache(Tuple1(v1)) { f(v1) }
  }

  private def cachedWithoutModificationCount0[T1, T2, R](id: String, name: String, wrapper: ValueWrapper[R], cleanupScheduler: CleanupScheduler, f: (T1, T2) => R): (T1, T2) => R = {
    val cache = new CacheWithoutModificationCountN[(T1, T2), R](id, name, wrapper, cleanupScheduler)
    (v1, v2) => cache((v1, v2)) { f(v1, v2) }
  }
}
