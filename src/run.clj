;;; From: http://www.hackers-with-attitude.com/2009/08/intertactive-programming-with-clojure.html
;;; Copyright Robin Brandt, freiheit.com.

(ns logikorr-jetty
  (:use logikorr.servlet)
  (:use compojure.server.jetty compojure.http compojure.control))

(defmacro with-app-engine
  "testing macro to create an environment for a thread"
  ([body]
     `(with-app-engine env-proxy ~body))
  ([proxy body]
     `(last (doall [(com.google.apphosting.api.ApiProxy/setEnvironmentForCurrentThread ~proxy)
                    ~body]))))

(defn login-aware-proxy
  "returns a proxy for the google apps environment that works locally"
  [request]
  (let [email (:email (:session request))]
    (proxy [com.google.apphosting.api.ApiProxy$Environment] []
      (isLoggedIn [] (boolean email))
      (getAuthDomain [] "")
      (getRequestNamespace [] "")
      (getDefaultNamespace [] "")
      (getAttributes [] (java.util.HashMap.))
      (getEmail [] (or email ""))
      (isAdmin [] true)
      (getAppId [] "local"))))

(defn environment-decorator
  "decorates the given application with a local version of the app engine environment"
  [application]
  (fn [request]
    (with-app-engine (login-aware-proxy request)
      (application request))))

(defn init-app-engine
  "Initialize the app engine services."
  ([]
     (init-app-engine "/tmp"))
  ([dir]
     (com.google.apphosting.api.ApiProxy/setDelegate
      (proxy [com.google.appengine.tools.development.ApiProxyLocalImpl] [(java.io.File. dir)]))))

;; make sure every thread has the environment set up

(defn start-logikorr []
  (init-app-engine)
  (run-server {:port 8080} "/*" (servlet (environment-decorator logikorr))))
