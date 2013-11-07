CREATE TABLE "RouteRatings"(
    "routeId"       INTEGER NOT NULL,
    "userId"        INTEGER NOT NULL,
    "rating"        INTEGER NOT NULL,
    "timeAdded"     TIMESTAMP DEFAULT CURRENT_TIMESTAMP() NOT NULL );

ALTER TABLE "RouteRatings" ADD FOREIGN KEY("userId") REFERENCES "Users"("id") ON UPDATE CASCADE;
ALTER TABLE "RouteRatings" ADD FOREIGN KEY("routeId") REFERENCES "Routes"("id") ON UPDATE CASCADE;
    
ALTER TABLE "RouteRatings" ADD CONSTRAINT "singleRatingPerUserConstraint" UNIQUE("routeId", "userId");

