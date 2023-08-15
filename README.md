# cl-opentelemetry

The purpose of this is to be a primitive means of providing the functionality for
tracing and sending those traces to <domain>/v1/traces so that they can be viewed by a
service like uptrace.
This is a HTTP JSON based implementation rather than a gRPC protobuf version :(

I tested all outputs generated using Uptrace as the collector, Shasht for encoding hash tables to JSON and Dexador for making my HTTP requests to Uptrace.


## What we have

- Context based traces
- Condition wrapping 

## How to

### Quick setup
To start create a subclass of `opentelemetry`

```lisp
(defclass my-opentelemetry (opentelemetry)
  ())
```

Then setf *ot* to an instance
```lisp
(setf *ot* (make-instance 'my-opentelemetry
                          :backgroundp t
                          :traces-url "my-traces-url"))
```


set `:backgroundp` to `nil` if you want telemetry to be sent to your collector in the current
thread rather than a new thread spawned for each collected trace.


Doing this allows you to specialize the behaviour of certain aspects of the protocol.
The specialized GF's are:
- `#'record` fallback method in src/background.lisp
- `#'handle-failed-request` default method in src/background.lisp
- `#'handle-execution-condition` default methods in src/trace.lisp

Next you need to specialize the method `make-request` so you can make HTTP requests.
An example using Dexador would look like:
```lisp
(defmethod make-request ((ot my-opentelemetry) content)
  ;;content is the json as a string
  (dex:post (traces-url ot)
            :headers '(("Content-Type" . "application/json"))
            :content content))
```
And finally plug in your favourite JSON writer.
An example using Shasht would look like:
```lisp
(defmethod write-json ((ot my-opentelemetry) json &optional stream)
  (let ((*print-pretty* nil))
    (shasht:write-json json stream)))
```
This library has only been tested with shasht!!! However the internal structures used to
represent traces are converted to hash-tables before being written as json.

Take a look in `src/examples/starting.lisp`




### How to create a trace

To start you can choose to make instances of two classes
- `scope`
- `resource`

and use the macro `with-new-trace` to initialize a trace.

```lisp
  (let ((scope (make-instance 'scope))
        (resource (make-instance 'resource :attributes '(:service.name "qtservice2"))))
    (with-new-trace (*trace* :scope scope :resource resource)
                    `(:service-id "r.y.m"
                      :status ,(to-status-code :ok)
                      :service-name "demonstration"
                      :telemetry-sdk-name "cl-opentelemetry"
                      :telemetry-sdk-version ,*version* 
                      :telemetry-sdk-language "Common Lisp")
        ... ))

```
The first two arguments are a symbol to bind to the newly create trace, and a symbol to bind to the newly created list of spans. The latter is used for putting new spans in the correct context. These are used later.

Next are two optional keyword arguments, your scope and resource. If you do not provide them
then defaults are created. You can create your own when you want to provide `attributes` or `values` to them.

- Attributes must be a plist whose keys are keywords. These keywords are translate to dot case when they are encoded.
- Values must also be a plist whose keys are keywords. These keywords are translated to camel case when they are encoded.

Classes like `span` have many Values that are slots. In this case use the `:values` initarg to provide key -> val mappings that are not standard.

To continue the last argument (not body) is a plist which contains shared-attributes, these attributes are appended to every instance of `span` and `event` made using `with-span` and `with-event` in the context of this `with-new-trace`.


Now you have your trace initialized you can start making use of the macros
- `with-add-span`
- `with-span`
- `with-add-event`
- `with-event`

Which I think is best demonstrated by example.
Take a look at `src/examples/trace.lisp`


### Exceptional situations


If an error occurs within the body of a `with-span/event` then the condition is handled by `handle-execution-condition` which will reinitialize the span/event with new attributes to try and display the condition in whatever collector you are using, by default I have methods that work for uptrace. See `src/trace.lisp`
The condition is then resignalled.
This allows exceptional situations to be recorded and forwarded to your collector hopefully
without disrupting the normal flow of operations.





## License

MIT

