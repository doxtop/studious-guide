## studious-guide 

Sell tikets. Probably not working now, didn't check...

 > sbt 'run filename query-date show-date'

and select `[1] t4s.Cli` option to run CLI application

`filename` expected to be absolute and `query-date` and `show-date` should be specified in ISO 8601 format YYYY-MM-DD.

Or 

 > sbt run

and select `[2] t4s.WebApp` option to run the web application on http://localhost:8080.

Host and port specified in `application.conf`.

## ux

Since ```<input date>``` is used for date selection use firefox/chrome for native date picker.

## Build ui (dev)

```bash
pulp build -t src/main/resources/app.js --skip-entry-point --main Schedule
```
