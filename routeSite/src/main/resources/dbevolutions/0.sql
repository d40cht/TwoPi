CREATE TABLE "Users"(
    "id" INTEGER PRIMARY KEY AUTO_INCREMENT,
    "extId" VARCHAR,
    "email" VARCHAR,
    "name" VARCHAR,
    "numLogins" INTEGER,
    "firstLogin" TIMESTAMP,
    "lastLogin" TIMESTAMP );
    
ALTER TABLE "Users" ADD CONSTRAINT "extId_unique" UNIQUE("extId");
