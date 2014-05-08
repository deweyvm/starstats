
import Database.HDBC
import Database.HDBC.ODBC

main :: IO ()
main = do
    let connectionString =  "DSN=name32;Driver={MySQL ODBC 5.3 ANSI Driver};Server=localhost;Port=3306;Database=testdb;User=root;Password=password;Option=3;"
    let ioconn = connectODBC connectionString
    conn <- ioconn
    vals <- quickQuery conn "SELECT version();" []
    print vals

