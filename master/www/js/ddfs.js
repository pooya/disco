var app = angular.module("DdfsApp", ["ngRoute"]);

app.controller("tagController", ["$http", "$scope", function($http, $scope) {
    $scope.selectedTag = "hello";
    $http.get("/ddfs/tags").then(function(res) {
        $scope.tags = res.data;                
    });
}]);
