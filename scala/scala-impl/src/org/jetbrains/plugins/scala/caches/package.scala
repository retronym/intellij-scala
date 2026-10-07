package org.jetbrains.plugins.scala

import com.intellij.openapi.util.ModificationTracker
import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.caches.CacheInUserData._
import org.jetbrains.plugins.scala.caches.CacheWithRecursionGuard.{cacheWithRecursionGuard0, cacheWithRecursionGuardN}
import org.jetbrains.plugins.scala.caches.stats.Tracer

package object caches {

  // TODO Detect control flow exceptions

  /** `cacheId[this.type, "getType"]`: the id and display name of a cache, computed at compile time. */
  inline def cacheId[T, N <: String & Singleton]: CacheId = CacheId.of[T, N]

  def cached[R](key: CacheId, modificationTracker: => ModificationTracker, f: () => R): () => R = {
    val cache = new Cache0[R](key.id, key.displayName, modificationTracker)
    () => cache { f() }
  }

  def cached[T1, R](key: CacheId, modificationTracker: => ModificationTracker, f: T1 => R): T1 => R = {
    val cache = new CacheN[Tuple1[T1], R](key.id, key.displayName, modificationTracker)
    v1 => cache(Tuple1(v1)) { f(v1) }
  }

  def cached[T1, T2, R](key: CacheId, modificationTracker: => ModificationTracker, f: (T1, T2) => R): (T1, T2) => R = {
    val cache = new CacheN[(T1, T2), R](key.id, key.displayName, modificationTracker)
    (v1, v2) => cache((v1, v2)) { f(v1, v2) }
  }

  def cached[T1, T2, T3, R](key: CacheId, modificationTracker: => ModificationTracker, f: (T1, T2, T3) => R): (T1, T2, T3) => R = {
    val cache = new CacheN[(T1, T2, T3), R](key.id, key.displayName, modificationTracker)
    (v1, v2, v3) => cache((v1, v2, v3)) { f(v1, v2, v3) }
  }

  def cached[T1, T2, T3, T4, R](key: CacheId, modificationTracker: => ModificationTracker, f: (T1, T2, T3, T4) => R): (T1, T2, T3, T4) => R = {
    val cache = new CacheN[(T1, T2, T3, T4), R](key.id, key.displayName, modificationTracker)
    (v1, v2, v3, v4) => cache((v1, v2, v3, v4)) { f(v1, v2, v3, v4) }
  }

  def cachedWithoutModificationCount[R](key: CacheId, wrapper: ValueWrapper[R], cleanupScheduler: CleanupScheduler, f: () => R): () => R = {
    val cache = new CacheWithoutModificationCount0[R](key.id, key.displayName, wrapper, cleanupScheduler)
    () => cache { f() }
  }

  def cachedWithoutModificationCount[T1, R](key: CacheId, wrapper: ValueWrapper[R], cleanupScheduler: CleanupScheduler, f: T1 => R): T1 => R = {
    val cache = new CacheWithoutModificationCountN[Tuple1[T1], R](key.id, key.displayName, wrapper, cleanupScheduler)
    v1 => cache(Tuple1(v1)) { f(v1) }
  }

  def cachedWithoutModificationCount[T1, T2, R](key: CacheId, wrapper: ValueWrapper[R], cleanupScheduler: CleanupScheduler, f: (T1, T2) => R): (T1, T2) => R = {
    val cache = new CacheWithoutModificationCountN[(T1, T2), R](key.id, key.displayName, wrapper, cleanupScheduler)
    (v1, v2) => cache((v1, v2)) { f(v1, v2) }
  }

  // TODO Factory method instead of the ProjectUserDataHolder type class

  def cachedInUserData[E: ProjectUserDataHolder, R](key: CacheId, dataHolder: E, dependency: => AnyRef)(f: => R): R =
    cacheInUserData0(key.id, key.displayName, dataHolder, dependency, f)

  def cachedInUserData[E: ProjectUserDataHolder, T <: Product, R](key: CacheId, dataHolder: E, dependency: => AnyRef, v: T)(f: => R): R =
    cacheInUserDataN[E, T, R](key.id, key.displayName, dataHolder, dependency, v, f)

  // TODO (defaultValue: => R) parameter list

  def cachedWithRecursionGuard[R](key: CacheId, element: PsiElement, defaultValue: => R, dependency: => AnyRef)(f: => R): R =
    cacheWithRecursionGuard0(key.id, key.displayName, element, defaultValue, dependency, f)

  def cachedWithRecursionGuard[T <: Product, R](key: CacheId, element: PsiElement, defaultValue: => R, dependency: => AnyRef, v: T)(f: => R): R =
    cacheWithRecursionGuardN[T, R](key.id, key.displayName, element, defaultValue, dependency, v, f)

  def measure[R](key: CacheId)(f: => R): R = {
    val tracer = Tracer(key.id, key.displayName)
    tracer.invocation()
    tracer.calculationStart()
    try {
      f
    } finally {
      tracer.calculationEnd()
    }
  }
}
