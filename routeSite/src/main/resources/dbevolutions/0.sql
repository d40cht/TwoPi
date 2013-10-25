CREATE TABLE "Users"(
    "id" INTEGER PRIMARY KEY AUTO_INCREMENT NOT NULL,
    "extId" VARCHAR NOT NULL,
    "name" VARCHAR NOT NULL,
    "email" VARCHAR NOT NULL,
    "numLogins" INTEGER NOT NULL,
    "firstLogin" TIMESTAMP NOT NULL,
    "lastLogin" TIMESTAMP NOT NULL );
    
ALTER TABLE "Users" ADD CONSTRAINT "extId_unique" UNIQUE("extId");

CREATE TABLE "Routes"(
    "id"            INTEGER PRIMARY KEY AUTO_INCREMENT NOT NULL,
    "routeData"     VARCHAR NOT NULL,
    "startLon"      REAL NOT NULL,
    "startLat"      REAL NOT NULL,
    "routeType"     VARCHAR NOT NULL,
    "distance"      REAL NOT NULL,
    "ascent"        REAL NOT NULL,
    "duration"      REAL NOT NULL,
    "timeAdded"     TIMESTAMP DEFAULT CURRENT_TIMESTAMP() NOT NULL,
    "userId"        INTEGER
);
ALTER TABLE "Routes" ADD FOREIGN KEY("userId") REFERENCES "Users"("id") ON UPDATE CASCADE;

CREATE TABLE "RouteNames"(
    "routeId"       INTEGER NOT NULL,
    "name"          VARCHAR NOT NULL,
    "description"   VARCHAR NOT NULL,
    "timeAdded"     TIMESTAMP DEFAULT CURRENT_TIMESTAMP() NOT NULL,
);
ALTER TABLE "RouteNames" ADD FOREIGN KEY("routeId") REFERENCES "Routes"("id") ON UPDATE CASCADE;

