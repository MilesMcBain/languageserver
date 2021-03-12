TCPLanguageClient <- R6::R6Class("TCPLanguageClient",
    inherit = LanguageBase,
    private = list(
        read_char_buf = raw(0)
    ),
    public = list(
        connection = NULL,
        rootUri = NULL,
        ClientCapabilities = NULL,
        ServerCapabilities = NULL,
        diagnostics = NULL,

        initialize = function(host = "localhost", port = NULL) {
            if (is.null(port)) stop("port required to connect over TCP")
            self$connection <- socketConnection(host = host, port = port, open = "r+b", server = TRUE)
            message("TCP client connected to ", paste0(host,":","port"))
            super$initialize()
        },

        finalize = function() {
            if (!is.null(self$connection)) {
                close(self$connection)
                self$connection <- NULL
            }
            super$finalize()
        },

        check_connection = function() {
            if (!is.null(self$connection) && !isOpen(self$connection))
                stop("Server is dead.")
        },

        write_text = function(text) {
            self$check_connection()
            writeLines(text, self$connection)
        },

        read_output_lines = function(timeout = 5) {
            self$check_connection()
            if (socketSelect(list(self$connection), timeout = timeout)) {
                    readLines(self$connection, encoding = "UTF-8")
                } else {
                    character(0)
                }
        },

        read_line = function() {
            self$check_connection()
            if (socketSelect(list(self$connection), timeout = 0)) {
                    readLines(self$connection, n = 1, encoding = "UTF-8")
                } else {
                    character(0)
                }
        },

        read_char = function(n) {
            self$check_connection()
            out <- readChar(self$connection, n, useBytes = TRUE)
            Encoding(out) <- "UTF-8"
            out
        },

        welcome = function() {
            self$deliver(
                self$request(
                    "initialize",
                    list(
                        rootUri = self$rootUri,
                        capabilities = self$ClientCapabilities
                    )
                ),
                callback = function(self, result) {
                    self$ServerCapabilities <- result$capabilities
                }
            )
        },

        start = function(working_dir = getwd(), capabilities = NULL) {
            self$rootUri <- path_to_uri(working_dir)
            self$ClientCapabilities <- capabilities
            self$welcome()
        },

        run = function() {
            # placeholder
        }
    )
)


TCPLanguageClient$set("public", "register_handlers", function() {
    self$request_handlers <- list()
    self$notification_handlers <- list()
})
