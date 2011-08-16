Warpy's Blitz on Rails server!


As part of my ongoing quest to do horrible things to blitzmax, I've written a ruby on rails-style server.
You just include "server.bmx", call start_server and run_server, and off it goes!
It has a rails-style routing syntax, so you can add routes which get mapped to particular controllers and views.

An example of a route is
	route.Create "/blog/:postid/:posttitle",["controller => blog", "action => view"]
	
The first argument is the route pattern - things with a colon in front match any text, and you can also use regexps by putting a \ in front of them.
The second argument is an extra set of labels you can define to help the system decide where to send the request. Every request needs at least a "controller" and an "action" label.

Once a route has been mapped, the system uses reflection to find the type whose name corresponds to the controller label, and a function, or view, belonging to that type whose name is the same as the action label.
That function should take one argument, of type HTTPSession.

A view's job is to do whatever action was requested, then provide the content of the page. The action part is up to you, but the rendering can either be done by directly calling s.render with the text you want, or by using render_template
All the labels from the routing phase are stored in the HTTPSession's "labels" map.

render_template takes one argument, the name of a .bhtml file (without the extension) found in the templates folder. It then fills in the template using the following ruby-like syntax:
Something of the form <%= label %> inserts the value of the given label, found in the HTTPSession's "info" map.
You can also do things like <%= object.name %> to access an object's fields (though you have to insert it into the info map)

There's only one control function so far, and that's <% for OBJECT in COLLECTION %> ..... <% endfor %>.
It works just like an EachIn in blitzmax, and should work with any collection.



Enjoy!
	cp