'use strict';

var controllers = angular.module("ddfsControllers", []);

controllers.controller("tagController", ["$http", "$scope", function($http, $scope) {
    $scope.selectedTag = "hello";
    $http.get("/ddfs/tags").then(function(res) {
        $scope.tags = res.data;                
    });
}]);

controllers.controller("selectedTagController", ["$http", "$scope", "$routeParams",
               function($http, $scope, $routeParams) {
    $scope.selectedTag = $routeParams.tag;
}]);
