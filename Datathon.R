presc<-prescription
exam_prescription$Date<-paste(exam_prescription$ISSUE_DATE,"/2018",sep="")
exam_prescription$Date<-as.Date(exam_prescription$ISSUE_DATE,tryFormats = c("%d/%m/%Y"))
exams2 <- subset(exam_prescription,EXAMINATION_ID=="CCB2A2BD4ED36C11101111926134476811801888")
presc$Date<-paste(presc$ISSUE_DATE,"/2018",sep="")
presc$Date<-as.Date(presc$ISSUE_DATE, tryFormats = c("%d/%m/%Y"))
presc$Date <- as.Date(paste(presc$ISSUE_DATE,"/2018",sep=""),tryFormats=c("%d/%m/%Y"))
exams$Date <- as.Date(paste(exams$ISSUE_DATE,"/2018",sep=""),tryFormats=c("%d/%m/%Y"))



for (i in 1:nrow(exams2)){
  exams2$FU[i] <- ifelse(sum(presc$Date[which(presc$PATIENT_ID==exams2$PATIENT_ID[i])[grep(exams$ICD10_CODE[i],
      presc$ICD10_CODE[which(presc$PATIENT_ID==exams2$PATIENT_ID[i])])]]> exams2$Date[i])>0,1,0)
  print(i)
}

for (i in 1:nrow(exams2)){
  exams2$FU2[i] <- ifelse(sum(presc$Date[which(presc$PATIENT_ID==exams2$PATIENT_ID[i])]> exams2$Date[i])>0,1,0)
  print(i)
}

# Select specific conditions.
exams2$Dizziness <- exams2$ICD10_CODE=="R42"
exams2$Headache <- exams2$ICD10_CODE=="R51" | exams2$ICD10_CODE=="G44"
exams2$MS <- exams2$ICD10_CODE=="G35"
exams2$Other <- !(Dizziness|Headache|MS)
sum(Other)
sum(MS)
sum(c(Other,Headache,MS,Dizziness))
sum(exams2$FU[exams2$Dizziness==TRUE])
sum(exams2$FU[exams2$Headache==TRUE])
sum(exams2$FU[exams2$MS==TRUE])
sum(exams2$FU[exams2$Other==TRUE])
sum(exams2$FU)

sum(exams2$FU2[exams2$Dizziness==TRUE & exams2$AgeCat01==0])
sum(exams2$FU2[exams2$Headache==TRUE])
sum(exams2$FU2[exams2$MS==TRUE])
sum(exams2$FU2[exams2$Other==TRUE])

sum(exams2$FU2[exams2$Dizziness==TRUE])
sum(exams2$FU2[exams2$Headache==TRUE])
sum(exams2$FU2[exams2$MS==TRUE])
sum(exams2$FU2[exams2$Other==TRUE])
sum(exams2$FU)
