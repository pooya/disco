var app = angular.module("DdfsApp", []);

app.controller("tagController", ["$http", "$scope", function($http, $scope) {
    $http.get("/ddfs/tags").then(function(res) {
        $scope.tags = res.data;                
    });
    console.log($scope.tags);
}]);
