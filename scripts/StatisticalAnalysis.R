##vowel labels
RecordingComparison["VowelIPA"] <- "placeholder"
RecordingComparison$VowelIPA[RecordingComparison$Segment == "AA1"] <- "ɑ"
RecordingComparison$VowelIPA[RecordingComparison$Segment == "AO0"] <- "ɑ"
RecordingComparison$VowelIPA[RecordingComparison$Segment == "AO1"] <- "ɑ"

RecordingComparison$VowelIPA[RecordingComparison$Segment == "AE1"] <- "æ"

RecordingComparison$VowelIPA[RecordingComparison$Segment == "AH1"] <- "ʌ"
RecordingComparison$VowelIPA[RecordingComparison$Segment == "AH2"] <- "ʌ"

RecordingComparison$VowelIPA[RecordingComparison$Segment == "AY1"] <- "ɑi"

RecordingComparison$VowelIPA[RecordingComparison$Segment == "EH1"] <- "ɛ"
RecordingComparison$VowelIPA[RecordingComparison$Segment == "EH2"] <- "ɛ"

RecordingComparison$VowelIPA[RecordingComparison$Segment == "ER0" & RecordingComparison$Speaker == "Chelsea"] <- "ɚ"
RecordingComparison$VowelIPA[RecordingComparison$Segment == "ER0" & RecordingComparison$Speaker == "Claire"] <- "ɜ"
RecordingComparison$VowelIPA[RecordingComparison$Segment == "ER0" & RecordingComparison$Speaker == "Natalie"] <- "ɚ"
RecordingComparison$VowelIPA[RecordingComparison$Segment == "ER1" & RecordingComparison$Speaker == "Chelsea"] <- "ɚ"
RecordingComparison$VowelIPA[RecordingComparison$Segment == "ER1" & RecordingComparison$Speaker == "Claire"] <- "ɜ"
RecordingComparison$VowelIPA[RecordingComparison$Segment == "ER1" & RecordingComparison$Speaker == "Natalie"] <- "ɚ"

RecordingComparison$VowelIPA[RecordingComparison$Segment == "EY"] <- "ei"
RecordingComparison$VowelIPA[RecordingComparison$Segment == "EY1"] <- "ei"
RecordingComparison$VowelIPA[RecordingComparison$Segment == "EY2"] <- "ei"

RecordingComparison$VowelIPA[RecordingComparison$Segment == "I"] <- "i"
RecordingComparison$VowelIPA[RecordingComparison$Segment == "IY1"] <- "i"
RecordingComparison$VowelIPA[RecordingComparison$Segment == "IH1"] <- "ɪ"
RecordingComparison$VowelIPA[RecordingComparison$Segment == "IH2"] <- "ɪ"
RecordingComparison$VowelIPA[RecordingComparison$Segment == "OW1"] <- "ou"
RecordingComparison$VowelIPA[RecordingComparison$Segment == "UW1"] <- "u"


##create subsetted data for each set of analyses
RecordingComparison[which(RecordingComparison$Phase != "Ph1" & RecordingComparison$WordType == "TargetWord" & RecordingComparison$Program != "ZoomOriginalAudio" & RecordingComparison$AllFactors != "NA_Zoom_macOSX_Local_mp4" & RecordingComparison$AllFactors != " NA_Zoom_macOSX_Remote_mp4" & RecordingComparison$AllFactors != "NA_Zoom_macOSX_Remote_no" & RecordingComparison$AllFactors != "NA_Zoom_windows_Remote_no" & RecordingComparison$AllFactors != "NA_Zoom_macOSX_Remote_mp4"),] -> Phase2

RecordingComparison[which(RecordingComparison$Phase != "Ph2" & RecordingComparison$Speaker != "Natalie" & RecordingComparison$WordType == "TargetWord"),] -> Phase1

RecordingComparison[which(RecordingComparison$Phase != "Ph1" & RecordingComparison$WordType == "TargetWord" & (RecordingComparison$Program == "Zoom"|RecordingComparison$Program == "ZoomOriginalAudio")),] -> ZoomConditions

#NB as.numeric(as.character(fzero_mean)) for when the column isn't being treated as numeric


##Phase 1 analyses

Phase1$Device <- relevel(as.factor(Phase1$Device), ref = "zoomH4n")

#consonant duration
summary(lmer(Duration~Device+(1|Speaker), data = Phase1[which(Phase1$ConsonantVsVowel == "consonant"),]))

#vowel duration
summary(lmer(Duration~Device+(1|Speaker), data = Phase1[which(Phase1$ConsonantVsVowel == "vowel"),]))

#f0 mean
summary(lmer(as.numeric(as.character(fzero_mean))~Device+(1|Speaker), data = Phase1[which(Phase1$ConsonantVsVowel == "vowel"),]))

#f0 peak timing
summary(lmer(maxTimeRelative~Device+(1|Speaker), data = Phase1[which(Phase1$ConsonantVsVowel == "vowel"),]))

#jitter
summary(lmer(as.numeric(as.character(jitter))~Device+(1|Speaker), data = Phase1[which(Phase1$ConsonantVsVowel == "vowel"),]))

#spectral tilt
summary(lmer(as.numeric(as.character(H1.H2))~Device+(1|Speaker), data = Phase1[which(Phase1$ConsonantVsVowel == "vowel"),]))

#HNR
summary(lmer(as.numeric(as.character(hnr))~Device+(1|Speaker), data = Phase1[which(Phase1$ConsonantVsVowel == "vowel"),]))

#F1
summary(lmer(F1~Device+(1|Speaker)+(1|VowelIPA), data = Phase1[which(Phase1$ConsonantVsVowel == "vowel"  & Phase1$VowelIPA != "ai"),]))

#F2
summary(lmer(F2~Device+(1|Speaker)+(1|VowelIPA), data = Phase1[which(Phase1$ConsonantVsVowel == "vowel"  & Phase1$VowelIPA != "ai"),]))

#center of gravity
summary(lmer(cog~Device+(1|Speaker)+(1|Segment), data = Phase1[which(Phase1$SegmentManner == "fricative"),]))

#vowel duration by stress
summary(lmer(Duration~Device*Stress+(1|Speaker), data = Phase1[which(Phase1$ConsonantVsVowel == "vowel" & Phase1$Stress != "NA"),]))

#F0 by stress
summary(lmer(as.numeric(as.character(fzero_mean))~Device*Stress+(1|Speaker), data = Phase1[which(Phase1$ConsonantVsVowel == "vowel" & Phase1$Stress != "NA"),]))

#vowel duration by coda voicing
summary(lmer(Duration~Device*FollowingVoicing+(1|Speaker), data = Phase1[which(Phase1$ConsonantVsVowel == "vowel"),]))

#HNR by coda voicing
summary(lmer(as.numeric(as.character(hnr))~Device*FollowingVoicing+(1|Speaker), data = Phase1[which(Phase1$ConsonantVsVowel == "vowel"),]))

#HNR by onset voicing
summary(lmer(as.numeric(as.character(hnr))~Device*PrecedingVoicing+(1|Speaker), data = Phase1[which(Phase1$ConsonantVsVowel == "vowel"),]))

#spectral tilt by onset voicing
summary(lmer(as.numeric(as.character(H1.H2))~Device*PrecedingVoicing+(1|Speaker), data = Phase1[which(Phase1$ConsonantVsVowel == "vowel"),]))

#f0 maximum by onset voicing
summary(lmer(as.numeric(as.character(fzero_high))~Device*PrecedingVoicing+(1|Speaker), data = Phase1[which(Phase1$ConsonantVsVowel == "vowel"),]))

#cog by segment
summary(lmer(cog~Device*Segment+(1|Speaker), data = Phase1[which(Phase1$Segment == "S"|Phase1$Segment == "SH"),]))

#F1 by vowel
lmer(F1~Device+Segment+(1|Speaker), data = Phase1[which(Phase1$ConsonantVsVowel == "vowel" & Phase1$VowelIPA != "ai"),]) -> model1
lmer(F1~Device*Segment+(1|Speaker), data = Phase1[which(Phase1$ConsonantVsVowel == "vowel" & Phase1$VowelIPA != "ai"),]) -> model1
anova(model1,model2)

#F2 by vowel
lmer(F2~Device+Segment+(1|Speaker), data = Phase1[which(Phase1$ConsonantVsVowel == "vowel" & Phase1$VowelIPA != "ai"),]) -> model1
lmer(F2~Device*Segment+(1|Speaker), data = Phase1[which(Phase1$ConsonantVsVowel == "vowel" & Phase1$VowelIPA != "ai"),]) -> model1
anova(model1,model2)



##Phase 2 analyses

Phase2$Program <- relevel(as.factor(Phase1$Program), ref = "zoomH4n")



#F1
summary(lmer(F1~Device+(1|Speaker)+(1|VowelIPA), data = Phase2[which(Phase2$ConsonantVsVowel == "vowel"  & Phase2$VowelIPA != "ai"),]))

#F2
summary(lmer(F2~Device+(1|Speaker)+(1|VowelIPA), data = Phase2[which(Phase2$ConsonantVsVowel == "vowel"  & Phase2$VowelIPA != "ai"),]))

#center of gravity
summary(lmer(cog~Device+(1|Speaker)+(1|Segment), data = Phase2[which(Phase2$SegmentManner == "fricative"),]))

#vowel duration by stress
summary(lmer(Duration~Device*Stress+(1|Speaker), data = Phase2[which(Phase2$ConsonantVsVowel == "vowel" & Phase2$Stress != "NA"),]))

#F0 by stress
summary(lmer(as.numeric(as.character(fzero_mean))~Device*Stress+(1|Speaker), data = Phase2[which(Phase2$ConsonantVsVowel == "vowel" & Phase2$Stress != "NA"),]))

#vowel duration by coda voicing
summary(lmer(Duration~Device*FollowingVoicing+(1|Speaker), data = Phase2[which(Phase2$ConsonantVsVowel == "vowel"),]))

#HNR by coda voicing
summary(lmer(as.numeric(as.character(hnr))~Device*FollowingVoicing+(1|Speaker), data = Phase2[which(Phase2$ConsonantVsVowel == "vowel"),]))

#HNR by onset voicing
summary(lmer(as.numeric(as.character(hnr))~Device*PrecedingVoicing+(1|Speaker), data = Phase2[which(Phase2$ConsonantVsVowel == "vowel"),]))

#spectral tilt by onset voicing
summary(lmer(as.numeric(as.character(H1.H2))~Device*PrecedingVoicing+(1|Speaker), data = Phase2[which(Phase2$ConsonantVsVowel == "vowel"),]))

#f0 maximum by onset voicing
summary(lmer(as.numeric(as.character(fzero_high))~Device*PrecedingVoicing+(1|Speaker), data = Phase2[which(Phase2$ConsonantVsVowel == "vowel"),]))

#cog by segment
summary(lmer(cog~Device*Segment+(1|Speaker), data = Phase2[which(Phase2$Segment == "S"|Phase2$Segment == "SH"),]))

#F1 by vowel
lmer(F1~Device+Segment+(1|Speaker), data = Phase2[which(Phase2$ConsonantVsVowel == "vowel" & Phase2$VowelIPA != "ai"),]) -> model1
lmer(F1~Device*Segment+(1|Speaker), data = Phase2[which(Phase2$ConsonantVsVowel == "vowel" & Phase2$VowelIPA != "ai"),]) -> model1
anova(model1,model2)

#F2 by vowel
lmer(F2~Device+Segment+(1|Speaker), data = Phase2[which(Phase2$ConsonantVsVowel == "vowel" & Phase2$VowelIPA != "ai"),]) -> model1
lmer(F2~Device*Segment+(1|Speaker), data = Phase2[which(Phase2$ConsonantVsVowel == "vowel" & Phase2$VowelIPA != "ai"),]) -> model1
anova(model1,model2)



##Zoom Conditions analyses

ZoomConditions$AllFactors <- relevel(as.factor(ZoomConditions$AllFactors), ref = "NA_Zoom_macOSX_Local_no")

#consonant duration
summary(lmer(Duration~AllFactors+(1|Speaker), data = Phase2[which(Phase2$ConsonantVsVowel == "consonant"),]))

#vowel duration
summary(lmer(Duration~AllFactors+(1|Speaker), data = Phase2[which(Phase2$ConsonantVsVowel == "vowel"),]))

#f0 mean
summary(lmer(as.numeric(as.character(fzero_mean))~AllFactors+(1|Speaker), data = Phase2[which(Phase2$ConsonantVsVowel == "vowel"),]))

#f0 peak timing
summary(lmer(maxTimeRelative~AllFactors+(1|Speaker), data = Phase2[which(Phase2$ConsonantVsVowel == "vowel"),]))

#jitter
summary(lmer(as.numeric(as.character(jitter))~AllFactors+(1|Speaker), data = Phase2[which(Phase2$ConsonantVsVowel == "vowel"),]))

#spectral tilt
summary(lmer(as.numeric(as.character(H1.H2))~AllFactors+(1|Speaker), data = Phase2[which(Phase2$ConsonantVsVowel == "vowel"),]))

#HNR
summary(lmer(as.numeric(as.character(hnr))~AllFactors+(1|Speaker), data = Phase2[which(Phase2$ConsonantVsVowel == "vowel"),]))

#F1
summary(lmer(F1~AllFactors+(1|Speaker)+(1|VowelIPA), data = ZoomConditions[which(ZoomConditions$ConsonantVsVowel == "vowel"),]))

#F2
summary(lmer(F2~AllFactors+(1|Speaker)+(1|VowelIPA), data = ZoomConditions[which(ZoomConditions$ConsonantVsVowel == "vowel"),]))

#center of gravity
summary(lmer(cog~AllFactors+(1|Speaker)+(1|Segment), data = ZoomConditions[which(ZoomConditions$SegmentManner == "fricative"),]))


