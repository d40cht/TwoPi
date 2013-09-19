
function localStorageGetOrElse( name, value )
{
    var existing = localStorage.getItem(name);
    if ( existing == null )
    {
        return value;
    }
    else
    {
        return existing;
    }
}

function localStorageWatch( scope, name )
{
    scope.$watch( name, function()
    {
        localStorage.setItem( name, scope[name] );
    } );
}

function RouteController($scope, $log)
{
    $scope.routeTypes = ["Walking", "Cycling"];
    
    $scope.distance = Number(localStorageGetOrElse('distance', 25.0));
    $scope.routeType = localStorageGetOrElse("routeType", $scope.routeTypes[0] );
    
    localStorageWatch( $scope, 'distance' );
    localStorageWatch( $scope, 'routeType' );
}
