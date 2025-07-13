module SqlHydra.Oracle.Provider

open SqlHydra.Domain

let provider = 
    {
        Provider.Id = "oracle"
        Provider.Name = "SqlHydra.Oracle"
        Provider.Type = Oracle
        Provider.DefaultReaderType = "Oracle.ManagedDataAccess.Client.OracleDataReader"
        Provider.DefaultProvider = "Oracle.ManagedDataAccess.Core"
        Provider.GetSchema = OracleSchemaProvider.getSchema
    }
