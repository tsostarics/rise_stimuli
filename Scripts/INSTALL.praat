
############### PRAAT VOCAL TOOLKIT INSTALLER
#
#
#   Welcome to the Praat Vocal Toolkit Installer.
#
#   Open this script with Praat and click "Run" > "Run".
#
#   If you already have the plugin installed
#       - a backup will be made in a folder named
#         "backed_up_plugin_VocalToolkit_XXXXXXXXXXXXXX";
#
#       - the installer will only update the necessary files,
#         keeping the ones you have created or added
#         (e.g. "eq" or "reverb" files)
#
#   For any question or suggestion, do not hesitate to contact me.
#
#       August 2022
#       Ramon Corretge, contact@praatvocaltoolkit.com
#
#
###############







if not unix and not macintosh and not windows
	beginPause: "Praat Vocal Toolkit Installer"
		comment: "This operating system is not supported by this installer."
		comment: "You can install the plugin manually. More information:"
		comment: "https://www.praatvocaltoolkit.com/download-installation.html"
	endPause: "OK", 1, 1
	exitScript()
endif

if fileReadable (defaultDirectory$ + "/plugin_VocalToolkit/setup.praat")
	newVersion$ = extractLine$(readFile$(defaultDirectory$ + "/plugin_VocalToolkit/setup.praat"), "Praat Vocal Toolkit. Version ")
else
	exitScript("Installation files not found")
endif

if fileReadable (preferencesDirectory$ + "/plugin_VocalToolkit/setup.praat")
	installedVersion$ = extractLine$(readFile$(preferencesDirectory$ + "/plugin_VocalToolkit/setup.praat"), "Praat Vocal Toolkit. Version ")
	if installedVersion$ == ""
		installedVersion$ = "-"
	endif
else
	installedVersion$ = "None"
endif

beginPause: "Praat Vocal Toolkit Installer"
	comment: "Installed version:  " + tab$ + installedVersion$
	comment: "New version:        " + tab$ + newVersion$
	comment: " "
	comment: "Click the 'Install' button to begin the installation."
clicked = endPause: "Cancel", "Install", 2, 1

if clicked = 1
	exitScript()
endif

if installedVersion$ <> "None"
	date# = date# ()
	uid$ = ""
	for i to size (date#)
		num$ = string$ (date# [i])
		if length (num$) == 1
			num$ = "0" + num$
		endif
		uid$ = uid$ + num$
	endfor
endif

if unix or macintosh
	newPluginFolder$ = defaultDirectory$ + "/plugin_VocalToolkit"
	currentPluginFolder$ = preferencesDirectory$ + "/plugin_VocalToolkit"

	if installedVersion$ <> "None"
		backupFolder$ = preferencesDirectory$ + "/backed_up_plugin_VocalToolkit_" + uid$
		cmd$ = "cp -R """ + currentPluginFolder$ + """ """ + backupFolder$ + """"
		runSystem: cmd$
	endif

	cmd$ = "rsync -a """ + newPluginFolder$ + "/"" """ + currentPluginFolder$ + """"
	runSystem: cmd$
endif

if windows
	newPluginFolder$ = defaultDirectory$ + "\plugin_VocalToolkit"
	currentPluginFolder$ = preferencesDirectory$ + "\plugin_VocalToolkit"

	if installedVersion$ <> "None"
		backupFolder$ = preferencesDirectory$ + "\backed_up_plugin_VocalToolkit_" + uid$
		cmd$ = "xcopy """ + currentPluginFolder$ + """ """ + backupFolder$ + """ /I /S"
		runSystem: cmd$
	endif

	cmd$ ="xcopy """ + newPluginFolder$ + """ """ + currentPluginFolder$ + """ /C /D /I /Q /S /Y"
	runSystem: cmd$
endif

beginPause: "Done!"
	comment: "Praat Vocal Toolkit has been installed."
	if installedVersion$ <> "None"
		comment: "The previous version of the plugin has been backed up to a folder named"
		comment: """backed_up_plugin_VocalToolkit_" + uid$ + """."
	endif
	comment: "Changes will take effect after restarting Praat."
clicked = endPause: "Quit Praat", "Finish", 2, 1

if clicked = 1
	Quit
endif






















############### PRAAT VOCAL TOOLKIT INSTALLER
#
#
#   Welcome to the Praat Vocal Toolkit Installer.
#
#   Open this script with Praat and click "Run" > "Run".
#
#   If you already have the plugin installed
#       - a backup will be made in a folder named
#         "backed_up_plugin_VocalToolkit_XXXXXXXXXXXXXX";
#
#       - the installer will only update the necessary files,
#         keeping the ones you have created or added
#         (e.g. "eq" or "reverb" files)
#
#   For any question or suggestion, do not hesitate to contact me.
#
#       August 2022
#       Ramon Corretge, contact@praatvocaltoolkit.com
#
#
###############
