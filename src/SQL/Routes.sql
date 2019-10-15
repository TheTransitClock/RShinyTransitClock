select r.shortName from Routes r where r.configRev=(select configRev from ActiveRevisions) order by r.shortName
