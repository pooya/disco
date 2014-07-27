'use strict';

var app = angular.module("ddfsApp", ["ngRoute", "ddfsControllers"]);

app.config(['$routeProvider', function($routeProvider) {
    $routeProvider.
    when("/", {
        templateUrl: "partials/ddfs_tags.html",
        controller: "tagController"
    }).
    when("/tag/:tag", {
        templateUrl: "partials/ddfs_tag.html",
        controller: "selectedTagController"
    }).
    otherwise({
        redirectTo: "/"
    });
}]);

