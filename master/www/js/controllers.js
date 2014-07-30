'use strict';

var controllers = angular.module("ddfsControllers", []);

controllers.controller("tagController", ["$http", "$scope", function($http, $scope) {
    $scope.selectedTag = "";
    $http.get("/ddfs/tags").then(function(res) {
        $scope.tags = res.data;                
    });

    $scope.getTag = function(tag) {
        $scope.selectedTag = tag;
        $http.get("ddfs/tag/" + $scope.selectedTag).then(function(res) {
            $scope.selectedTag = res.data;
        });
    };
}]);
