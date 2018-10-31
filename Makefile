################################################################################
# Make file to copy thesis files from original documents
# David Hope 
# October 25, 2018
# 
# 
################################################################################
all:copythesis  # default rule DO NOT EDIT
################################################################################
CH2LOC  := ../Hope.wesa/Model_Description_Chapter/masterfiles/
CH3LOC  := ../Hope.wesa/Confrontation_chapter/masterfiles/
CH4LOC  := ../../SharingFiles_ForWindows/EasternAnalysis/ThesisChapter/
CH5LOC  := ../../SharingFiles_ForWindows/CWS.Mansucript/Thesis\ Version/
CH6LOC  := ../Hope.wesa/FullThesis/master/
##### Explicit Dependencies #####
################################################################################

abstractfrontmatter:
	cp $(CH6LOC)HopeThesis.Rnw .
	cp $(CH6LOC)SoftwareUsed.Rnw .
	cp $(CH6LOC)../R.bib ./bibfiles/
	cp $(ch6LOC)../../library_thesis.bib ./bibfiles/

chapter1:
	cp $(CH6LOC)GeneralIntroduction.Rnw ./Chapter_1/knitr/
	# cp ./master/SoftwareUsed.Rnw .
	# cp ./master/GeneralDiscussiontex.Rnw .
	# cp ./master/GeneralIntroduction.* .
	# cp ../Model_Description_Chapter/masterfiles/* ../Model_Description_Chapter/.
	# cp ../Confrontation_chapter/masterfiles/* ../Confrontation_chapter/.
	# cp ~/Documents/SFU/PhD/SharingFiles_ForWindows/EasternAnalysis/ThesisChapter/* ~/Documents/SFU/PhD/SharingFiles_ForWindows/EasternAnalysis/buildthesischapter/.

chapter2: 
	cp $(CH2LOC)Introduction.tex ./Chapter_2/knitr/
	cp $(CH2LOC)ModelWalkthrough.Rnw ./Chapter_2/knitr/
	cp $(CH2LOC)SESA.Rnw ./Chapter_2/knitr/
	cp $(CH2LOC)Discussion.tex ./Chapter_2/knitr/
	cp $(CH2LOC)Appendices.Rnw ./Chapter_2/knitr/

chapter3:
	cp $(CH3LOC)Introduction_Confrontation.Rnw ./Chapter_3/knitr/
	cp $(CH3LOC)Confrontation_chapter.Rnw ./Chapter_3/knitr/
	cp $(CH3LOC)Discussion_Confrontation.Rnw ./Chapter_3/knitr/
	cp $(CH3LOC)Appendices_Confrontation.Rnw ./Chapter_3/knitr/

chapter4:
	cp $(CH4LOC)SESA_intro.tex ./Chapter_4/knitr/
	cp $(CH4LOC)Chapter4_SESA.Rnw ./Chapter_4/knitr/
	cp $(CH4LOC)SESA_Discussion.tex ./Chapter_4/knitr/
	cp $(CH4LOC)Appendix_SESA.Rnw ./Chapter_4/knitr/

chapter5:
	cp $(CH5LOC)ProgressionChapter.Rnw ./Chapter_5/knitr/

chapter6:
	cp $(CH6LOC)GeneralDiscussiontex.Rnw ./Chapter_1/knitr/


copythesis: abstractfrontmatter chapter1 chapter2 chapter3 chapter4 chapter5 chapter6
