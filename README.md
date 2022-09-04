# ring.jmx

A management UI in for [Ring](https://github.com/ring-clojure/ring) to access [Java Management Extensions](https://docs.oracle.com/javase/8/docs/api/javax/management/package-summary.html#package.description).

A Clojure library designed to ... well, that part is up to you.

## Usage

This library provides a ring handler wrapper function. 

```
(require '[ring.jmx])

(defn default-handler [request]
  {:status 200
   :body "<a href=\"/jmx/\">Go to the JMX UI!</a>"
   :headers {"content-type" "text/html"}})

(def my-app (wrap-jmx my-handler))
```

### Testing

To start the test application, type the following in the project folder:

```
lein with-profile +test ring server
```

Then open your browser at `http://localhost:3000/jmx`.

## License

Copyright © 2022 Janos Erdos

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
