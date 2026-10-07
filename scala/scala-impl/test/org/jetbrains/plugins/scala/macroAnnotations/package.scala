package org.jetbrains.plugins.scala

import com.intellij.openapi.project.Project
import com.intellij.psi.PsiManager
import org.jetbrains.plugins.scala.caches.stats.Tracer
import org.junit.Assert._

import scala.jdk.CollectionConverters._

package object macroAnnotations {
  def incModCount(project: Project): Unit = {
    val manager = PsiManager.getInstance(project)
    // TODO manager.getModificationTracker.asInstanceOf[PsiModificationTrackerImpl].incCounter()
    manager.dropPsiCaches()
  }

  def checkTracer(id: String, name: String, totalCount: Int, actualCount: Int)(body: => Unit): Unit = {
    Tracer.clearAll()
    Tracer.setEnabled(true)
    try {
      body
      checkTracerHas(id, name, totalCount, actualCount)
    } finally {
      Tracer.setEnabled(false)
    }
  }

  private def checkTracerHas(id: String, name: String, totalCount: Int, actualCount: Int): Unit = {
    val allData = Tracer.getCurrentData.asScala
    val data = allData.find(_.id == id).getOrElse {
      throw new AssertionError(s"No tracer data with id $id (${allData.map(_.id).mkString(", ")})")
    }
    assertEquals("Wrong name:", name, data.name)
    assertEquals("Wrong total count:", totalCount, data.totalCount)
    assertEquals("Wrong number of actual computations:", actualCount, data.actualCount)
  }
}
