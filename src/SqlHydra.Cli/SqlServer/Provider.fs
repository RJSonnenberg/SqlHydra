module SqlHydra.SqlServer.Provider

open SqlHydra.Domain

let provider = 
    {
        Provider.Id = "mssql"
        Provider.Name = "SqlHydra.SqlServer"
        Provider.Type = SqlServer
        Provider.DefaultReaderType = "Microsoft.Data.SqlClient.SqlDataReader"
        Provider.DefaultProvider = "Microsoft.Data.SqlClient"
        Provider.GetSchema = SqlServerSchemaProvider.getSchema
    }
    
    