# required: RGtk2
calculateGUI <- function() {
	library(RGtk2)

	
	kill <- function() {
		url <- Url$getText()
		
	}
		

	window <- gtkWindow()
	window["title"] <- "KillSomebody"

	frame <- gtkFrameNew("let's do it")
	window$add(frame)

	vbox <- gtkVBoxNew(FALSE, 8)
	vbox$setBorderWidth(25)
	frame$add(vbox)
	
	hbox <- gtkHBoxNew(FALSE, 8)
	vbox$packStart(hbox, FALSE, FALSE, 0)

	label <- gtkLabelNewWithMnemonic("_Url:")
	hbox$packStart(label,FALSE,FALSE,0)
	
	Url <- gtkEntryNew()
	Url$setWidthChars(50)
	label$setMnemonicWidget(Url)
	hbox$packStart(Url,FALSE,FALSE,0)


	vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)
	
	hbox <- gtkHBoxNew(FALSE,8)
	vbox$packStart(hbox, FALSE, FALSE, 0)
	
	label <- gtkLabelNewWithMnemonic("Save _Results?")
	hbox$packStart(label,FALSE,FALSE,0)
	
	toSave <- gtkCheckButton()
	hbox$packStart(toSave,FALSE,FALSE,0)
	
	label$setMnemonicWidget(toSave)
	label <- gtkLabelNewWithMnemonic("_Export file name?")
	hbox$packStart(label,FALSE,FALSE,0)
	
	exportFileName <- gtkEntryNew()
	exportFileName$setWidthChars(19)
	exportFileName$setText("outputs")
	hbox$packStart(exportFileName,FALSE,FALSE,0)
	
	label$setMnemonicWidget(exportFileName)
	label <- gtkLabel(".jpg")
	hbox$packStart(label,FALSE,FALSE,0)
	
	# Add button
	buttons <- gtkHButtonBoxNew()
	buttons$setBorderWidth(5)
	vbox$add(buttons)
	buttons$setLayout("spread")
	buttons$setSpacing(50)	
	
	buttonOK <- gtkButtonNewFromStock("gtk-ok")
	gSignalConnect(buttonOK, "clicked", kill)
	buttons$packStart(buttonOK,fill=F)
	
	buttonCancel <- gtkButtonNewFromStock("gtk-close")
	gSignalConnect(buttonCancel, "clicked", window$destroy)
	buttons$packStart(buttonCancel,fill=F)
}
