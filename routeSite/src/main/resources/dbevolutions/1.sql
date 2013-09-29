CREATE TABLE "Routes"(
    "id"            INTEGER PRIMARY KEY AUTO_INCREMENT,
    "routeData"     VARCHAR
);

CREATE TABLE "UserRoutes"(
    "routeId"       INTEGER,
    "userId"        INTEGER,
    "routeName"     VARCHAR
);

ALTER TABLE "UserRoutes" ADD FOREIGN KEY("routeId") REFERENCES "Routes"("id") ON UPDATE CASCADE;
ALTER TABLE "UserRoutes" ADD FOREIGN KEY("userId") REFERENCES "Users"("id") ON UPDATE CASCADE;
