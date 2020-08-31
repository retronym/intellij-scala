package org.jetbrains.plugins.scala
package project
package settings

import com.intellij.codeInsight.daemon.DaemonCodeAnalyzer
import com.intellij.compiler.server.BuildManager
import com.intellij.openapi.options.Configurable
import com.intellij.openapi.options.Configurable.Composite
import com.intellij.openapi.project.Project
import javax.swing.JPanel
import org.jetbrains.plugins.scala.statistics.{FeatureKey, Stats}

class ScalaCompilerConfigurable(project: Project)
  extends AbstractConfigurable(ScalaCompilerConfigurable.Name)
    with Composite {

  private val form = new ScalaCompilerConfigurationPanel(project)

  private def configuration = ScalaCompilerConfiguration.instanceIn(project)
  
  private val profilesPanel: ScalaCompilerProfilesPanel = form.getProfilesPanel

  override def createComponent(): JPanel = form.getContentPanel

  override def isModified: Boolean = {
    if (form.getIncrementalityType != configuration.incrementalityType)
      return true
    if (!equalSettings(profilesPanel.getDefaultProfile, configuration.defaultProfile))
      return true
    if (!profilesPanel.getModuleProfiles.corresponds(configuration.customProfiles)(equalSettings))
      return true

    false
  }

  private def equalSettings(profile1: ScalaCompilerSettingsProfile, profile2: ScalaCompilerSettingsProfile): Boolean =
    settingsState(profile1) == settingsState(profile2)

  private def settingsState(profile: ScalaCompilerSettingsProfile): ScalaCompilerSettingsState =
    profile.getSettings.toState

  override def reset(): Unit = {
    form.setIncrementalityType(configuration.incrementalityType)
    profilesPanel.initProfiles(configuration.defaultProfile, configuration.customProfiles)
  }

  override def apply(): Unit = {
    val newIncType = form.getIncrementalityType
    if (newIncType != configuration.incrementalityType) {
      Stats.trigger(FeatureKey.incrementalTypeSet(newIncType.name()))
    }

    configuration.incrementalityType = newIncType
    configuration.defaultProfile = profilesPanel.getDefaultProfile.copy
    configuration.customProfiles = profilesPanel.getModuleProfiles.map(_.copy)
    DaemonCodeAnalyzer.getInstance(project).restart()
    BuildManager.getInstance().clearState(project)
  }

  override def getConfigurables: Array[Configurable] = Array()
}

object ScalaCompilerConfigurable {
  def Name: String = ScalaBundle.message("scala.compiler")
}