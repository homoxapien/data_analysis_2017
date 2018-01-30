connString <- paste(
	"Server=msedxeus.database.windows.net",
	"Database=DAT209x01",
	"uid=RLogin",
	"pwd=P@sswOrd",
	"Driver={SQL Server}",
	sep=";")
conn <- odbcDriverConnect(connString)