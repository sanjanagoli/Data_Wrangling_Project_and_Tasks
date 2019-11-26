SELECT * INTO [sgoli].Dem_Con_TextMess FROM Demographics 
INNER JOIN 
    TextMessages ON Demographics.contactid=TextMessages.tri_contactId 
INNER JOIN 
    Conditions ON TextMessages.tri_contactId=Conditions.tri_patientid

SELECT dct.* INTO [sgoli].Dem_Con_Text_MaxDate 
FROM [sgoli].Dem_Con_TextMess dct
INNER JOIN (
	SELECT contactid, MAX(convert(date, TextSentDate, 1)) AS MaxDateTime
    FROM [sgoli].Dem_Con_TextMess
	GROUP BY contactid) groupedDct
ON dct.contactid = groupedDct.contactid 
AND dct.TextSentDate = groupedDct.MaxDateTime


select contactId, TextSentDate, gendercode, tri_age, parentcustomeridname, tri_imaginecareenrollmentstatus, 
    address1_stateorprovince, tri_enrollmentcompletedate, tri_imaginecareenrollmentemailsentdate,
    STRING_AGG(SenderName, ', ') as SenderName, STRING_AGG(tri_name, ', ') as tri_name
FROM [sgoli].Dem_Con_Text_MaxDate 
GROUP BY contactId, TextSentDate, gendercode, tri_age, parentcustomeridname, tri_imaginecareenrollmentstatus, 
    address1_stateorprovince, tri_enrollmentcompletedate, tri_imaginecareenrollmentemailsentdate

