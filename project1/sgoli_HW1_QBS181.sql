-- PROBLEM 1

sp_rename '[sgoli].Demographics.tri_age', 'Age', 'COLUMN';
GO
sp_rename '[sgoli].Demographics.gendercode', 'Gender', 'COLUMN';
GO
sp_rename '[sgoli].Demographics.contactid', 'ID', 'COLUMN';
GO
sp_rename '[sgoli].Demographics.address1_stateorprovince', 'State', 'COLUMN';
GO
sp_rename '[sgoli].Demographics.tri_imaginecareenrollmentemailsentdate', 'EmailSentDate', 'COLUMN';
GO
sp_rename '[sgoli].Demographics.tri_enrollmentcompletedate', 'Completedate', 'COLUMN';
GO

-- Calculate days until completion from email sent date
ALTER TABLE [sgoli].Demographics 
ADD DaysUntilCompletion int;

UPDATE [sgoli].Demographics 
SET 
    DaysUntilCompleted = DATEDIFF(day, try_convert(date, EmailSentDate, 101), try_convert(date, Completedate, 101))
WHERE
    Completedate <> 'NULL'


-- PROBLEM 2

ALTER TABLE [sgoli].Demographics
ADD EnrollmentStatus VARCHAR(255);

UPDATE [sgoli].Demographics
SET
    EnrollmentStatus = 
        CASE 
            WHEN tri_imaginecareenrollmentstatus = 167410011 THEN 'Complete'
            WHEN tri_imaginecareenrollmentstatus = 167410001 THEN 'Email Sent'
            WHEN tri_imaginecareenrollmentstatus = 167410004 THEN 'Non Responder'
            WHEN tri_imaginecareenrollmentstatus = 167410005 THEN 'Facilitated Enrollment'
            WHEN tri_imaginecareenrollmentstatus = 167410002 THEN 'Incomplete Enrollments'
            WHEN tri_imaginecareenrollmentstatus = 167410003 THEN 'Opted Out'
            WHEN tri_imaginecareenrollmentstatus = 167410000 THEN 'Unprocessed'
            WHEN tri_imaginecareenrollmentstatus = 167410006 THEN 'Second email sent'
        END
WHERE
    parentcustomeridname = 'Dartmouth-Hitchcock'

-- PROBLEM 3

ALTER TABLE [sgoli].Demographics
ADD Sex VARCHAR(255)

UPDATE [sgoli].Demographics
SET
    Sex =
    CASE
        WHEN Gender = '2' THEN 'Female'
        WHEN Gender = '1' THEN 'Male'
        WHEN Gender = '167410000' THEN 'Other'
        WHEN Gender = 'NULL' THEN 'Unknown'
    END
    
-- PROBLEM 4
ALTER TABLE [sgoli].Demographics
ADD AgeGroup VARCHAR(255)

UPDATE [sgoli].Demographics
SET
    AgeGroup = 
    CASE
        WHEN age BETWEEN 0 AND 25 THEN '0-25'
        WHEN age BETWEEN 26 AND 50 THEN '26-50'
        WHEN age BETWEEN 51 AND 75 THEN '51-75'
        WHEN age BETWEEN 76 AND 100 THEN '76-100'
        WHEN age BETWEEN 101 AND 125 THEN '101-125'
    END
 
