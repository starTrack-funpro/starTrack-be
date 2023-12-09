# starTrack - Backend

Part of Functional Programming Odd Semester 2023/2024 Final Project

by: bonaventuragal

## Overview

starTrack is a web based series (TV Series, Film, Comic, Novel) progress tracker. Built using Haskell.

## How To Run

### Prerequisites and Setup
1. Install `haskell` and `cabal`.
2. Install `PostgreSQL`.
3. Clone this repository.
4. Create a `PostgreSQL` database based on the file `startrack.sql`. Alternatively, run `psql -f startrack.sql` (assuming the `PostgreSQL` user is `postgres`).
5. Create a `.env` file at the root of the project.
6. Fill the `.env` file with:
```env
DATABASE_URL=**YOUR DATABASE CONNECTION URL**
JWT_SECRET=**A SECRET FOR JWT**
```

Set the values according to your specific configuration. For example:

```env
DATABASE_URL=postgresql://username:password@localhost:5432/startrack
JWT_SECRET=mysecret
```

### Running the Server
Run `cabal run` at the root of the project to start the server. The server will run at port `8000`. You can send a GET request to `localhost:8000/hello` to check the server.

### Admin User
The `startrack.sql` contains an SQL query to add an admin account with username `admin` and password `admin`. This account can be used to login as admin on the web.