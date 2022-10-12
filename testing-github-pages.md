testing github pages
================
Brant Armstrong
2022-10-12

``` r
rmarkdown::render
```

    ## function (input, output_format = NULL, output_file = NULL, output_dir = NULL, 
    ##     output_options = NULL, output_yaml = NULL, intermediates_dir = NULL, 
    ##     knit_root_dir = NULL, runtime = c("auto", "static", "shiny", 
    ##         "shinyrmd", "shiny_prerendered"), clean = TRUE, params = NULL, 
    ##     knit_meta = NULL, envir = parent.frame(), run_pandoc = TRUE, 
    ##     quiet = FALSE, encoding = "UTF-8") 
    ## {
    ##     perf_timer_start("render")
    ##     init_render_context()
    ##     on.exit(clear_render_context(), add = TRUE)
    ##     .globals$level <- .globals$level + 1L
    ##     on.exit({
    ##         .globals$level <- .globals$level - 1L
    ##         if (.globals$level == 0) clean_tmpfiles()
    ##     }, add = TRUE)
    ##     if (identical(output_format, "all")) {
    ##         output_format <- enumerate_output_formats(input)
    ##         if (is.null(output_format)) 
    ##             output_format <- "html_document"
    ##     }
    ##     if (is.character(output_format) && length(output_format) > 
    ##         1) {
    ##         outputs <- character()
    ##         for (i in seq_along(output_format)) {
    ##             output <- render(input = input, output_format = output_format[i], 
    ##                 output_file = output_file[i], output_dir = output_dir, 
    ##                 output_options = output_options, intermediates_dir = intermediates_dir, 
    ##                 knit_root_dir = knit_root_dir, runtime = runtime, 
    ##                 clean = clean, params = params, knit_meta = knit_meta, 
    ##                 envir = envir, run_pandoc = run_pandoc, quiet = quiet)
    ##             outputs <- c(outputs, output)
    ##         }
    ##         return(invisible(outputs))
    ##     }
    ##     if (run_pandoc) {
    ##         required_pandoc <- "1.12.3"
    ##         pandoc_available(required_pandoc, error = TRUE)
    ##     }
    ##     intermediates <- c()
    ##     on.exit(if (clean) unlink(intermediates, recursive = TRUE), 
    ##         add = TRUE)
    ##     if (!is.null(intermediates_dir)) {
    ##         if (!dir_exists(intermediates_dir)) 
    ##             dir.create(intermediates_dir, recursive = TRUE)
    ##         intermediates_dir <- normalize_path(intermediates_dir)
    ##     }
    ##     intermediates_loc <- function(file) {
    ##         if (is.null(intermediates_dir)) 
    ##             file
    ##         else file.path(intermediates_dir, file)
    ##     }
    ##     if (!is.null(output_dir)) {
    ##         if (!dir_exists(output_dir)) 
    ##             dir.create(output_dir, recursive = TRUE)
    ##         output_dir <- normalize_path(output_dir)
    ##     }
    ##     requires_knit <- tolower(xfun::file_ext(input)) %in% c("r", 
    ##         "rmd", "rmarkdown", "qmd")
    ##     original_input <- normalize_path(input)
    ##     if (grepl(.shell_chars_regex, basename(input))) {
    ##         input_no_shell_chars <- intermediates_loc(file_name_without_shell_chars(basename(input)))
    ##         if (file.exists(input_no_shell_chars)) {
    ##             stop2("The name of the input file cannot contain the special shell ", 
    ##                 "characters: ", .shell_chars_regex, " (attempted to copy to a ", 
    ##                 "version without those characters '", input_no_shell_chars, 
    ##                 "' ", "however that file already exists)")
    ##         }
    ##         file.copy(input, input_no_shell_chars, overwrite = TRUE)
    ##         intermediates <- c(intermediates, input_no_shell_chars)
    ##         input <- input_no_shell_chars
    ##         if (is.null(intermediates_dir)) {
    ##             intermediates_dir <- dirname(normalize_path(input_no_shell_chars))
    ##         }
    ##     }
    ##     if (!is.null(intermediates_dir) && same_path(intermediates_dir, 
    ##         dirname(original_input))) 
    ##         intermediates_dir <- NULL
    ##     force(knit_root_dir)
    ##     oldwd <- setwd(dirname(abs_path(input)))
    ##     on.exit(setwd(oldwd), add = TRUE)
    ##     input <- basename(input)
    ##     knit_input <- input
    ##     knit_output <- intermediates_loc(file_with_meta_ext(input, 
    ##         "knit", getOption("rmarkdown.knit.ext", "md")))
    ##     intermediates <- c(intermediates, knit_output)
    ##     md_input <- identical(tolower(xfun::file_ext(input)), "md")
    ##     if (identical(tolower(xfun::file_ext(input)), "r")) {
    ##         spin_input <- intermediates_loc(file_with_meta_ext(input, 
    ##             "spin", "R"))
    ##         file.copy(input, spin_input, overwrite = TRUE)
    ##         intermediates <- c(intermediates, spin_input)
    ##         spin_rmd <- knitr::spin(spin_input, knit = FALSE, envir = envir, 
    ##             format = "Rmd")
    ##         intermediates <- c(intermediates, spin_rmd)
    ##         knit_input <- spin_rmd
    ##         meta1 <- yaml_front_matter(knit_input)
    ##         meta2 <- list(title = input, author = Sys.info()[["user"]], 
    ##             date = as.character(Sys.Date()))
    ##         for (i in names(meta2)) if (!is.null(meta1[[i]])) 
    ##             meta2[[i]] <- NULL
    ##         if (length(meta2)) {
    ##             input_lines <- read_utf8(knit_input)
    ##             write_utf8(c(input_lines, "\n\n---", yaml::as.yaml(meta2), 
    ##                 "---"), knit_input)
    ##         }
    ##     }
    ##     input_lines <- read_utf8(knit_input)
    ##     front_matter <- parse_yaml_front_matter(input_lines)
    ##     old_output_metadata <- output_metadata$get()
    ##     on.exit(output_metadata$restore(old_output_metadata), add = TRUE)
    ##     output_metadata$restore(as.list(front_matter[["rmd_output_metadata"]]))
    ##     shiny_prerendered_dependencies <- list()
    ##     if (requires_knit && is_shiny_prerendered(front_matter$runtime, 
    ##         front_matter$server)) {
    ##         if (requireNamespace("shiny")) {
    ##             if (!"package:shiny" %in% search()) 
    ##                 attachNamespace("shiny")
    ##         }
    ##         else stop("The shiny package is required for shiny documents")
    ##         global_r <- file.path.ci(".", "global.R")
    ##         if (file.exists(global_r)) {
    ##             source(global_r, local = envir)
    ##         }
    ##         output_options$self_contained <- FALSE
    ##         output_options$dependency_resolver <- function(deps) {
    ##             shiny_prerendered_dependencies <<- list(deps = deps, 
    ##                 packages = get_loaded_packages())
    ##             list()
    ##         }
    ##     }
    ##     if (!is_output_format(output_format)) {
    ##         output_format <- output_format_from_yaml_front_matter(input_lines, 
    ##             output_options, output_format, output_yaml, output_file)
    ##         output_format <- create_output_format(output_format$name, 
    ##             output_format$options)
    ##     }
    ##     pandoc_to <- output_format$pandoc$to
    ##     output_auto <- pandoc_output_file(input, output_format$pandoc)
    ##     if (is.null(output_file) || is.na(output_file)) 
    ##         output_file <- output_auto
    ##     else {
    ##         if (!inherits(output_file, "AsIs") && xfun::file_ext(output_file) == 
    ##             "") 
    ##             output_file <- paste(output_file, xfun::file_ext(output_auto), 
    ##                 sep = ".")
    ##     }
    ##     if (!is.null(output_dir)) {
    ##         output_file <- file.path(output_dir, basename(output_file))
    ##     }
    ##     output_dir <- dirname(output_file)
    ##     if (!dir_exists(output_dir)) {
    ##         stop2("The directory '", output_dir, "' does not not exist.")
    ##     }
    ##     files_dir_slash <- file.path(output_dir, knitr_files_dir(basename(output_file)))
    ##     files_dir <- pandoc_path_arg(files_dir_slash)
    ##     cache_dir <- NULL
    ##     if (!is.null(intermediates_dir) && !is.null(output_format$intermediates_generator)) {
    ##         intermediates <- c(intermediates, output_format$intermediates_generator(original_input, 
    ##             intermediates_dir))
    ##     }
    ##     old_knit_meta <- knit_meta_reset()
    ##     on.exit({
    ##         knit_meta_reset()
    ##         if (length(old_knit_meta)) {
    ##             knitr::knit_meta_add(old_knit_meta, attr(old_knit_meta, 
    ##                 "knit_meta_id"))
    ##         }
    ##     }, add = TRUE)
    ##     runtime <- match.arg(runtime)
    ##     if (identical(runtime, "auto")) {
    ##         if (is_shiny_prerendered(front_matter$runtime, front_matter$server)) {
    ##             runtime <- "shiny_prerendered"
    ##         }
    ##         else {
    ##             runtime <- front_matter$runtime %||% "static"
    ##         }
    ##     }
    ##     context <- render_context()
    ##     context$df_print <- resolve_df_print(output_format$df_print)
    ##     env <- environment(render)
    ##     metadata_this <- env$metadata
    ##     do.call("unlockBinding", list("metadata", env))
    ##     on.exit({
    ##         if (bindingIsLocked("metadata", env)) {
    ##             do.call("unlockBinding", list("metadata", env))
    ##         }
    ##         env$metadata <- metadata_this
    ##         lockBinding("metadata", env)
    ##     }, add = TRUE)
    ##     env$metadata <- front_matter
    ##     if (!is.null(output_format$pre_knit)) {
    ##         output_format$pre_knit(input = original_input)
    ##     }
    ##     call_post_knit_handler <- function() {
    ##         if (!is.null(output_format$post_knit)) {
    ##             post_knit_extra_args <- output_format$post_knit(front_matter, 
    ##                 knit_input, runtime, encoding = "UTF-8")
    ##         }
    ##         else {
    ##             post_knit_extra_args <- NULL
    ##         }
    ##         c(output_format$pandoc$args, post_knit_extra_args)
    ##     }
    ##     id_prefix <- id_prefix_from_args(output_format$pandoc$args)
    ##     if (!nzchar(id_prefix) && is_shiny(runtime, front_matter[["server"]])) {
    ##         id_prefix <- "section-"
    ##         output_format$pandoc$args <- c(output_format$pandoc$args, 
    ##             rbind("--id-prefix", id_prefix))
    ##     }
    ##     if (requires_knit) {
    ##         optk <- knitr::opts_knit$get()
    ##         on.exit(knitr::opts_knit$restore(optk), add = TRUE)
    ##         optc <- knitr::opts_chunk$get()
    ##         on.exit(knitr::opts_chunk$restore(optc), add = TRUE)
    ##         hooks <- knitr::knit_hooks$get()
    ##         on.exit(knitr::knit_hooks$restore(hooks), add = TRUE)
    ##         ohooks <- knitr::opts_hooks$get()
    ##         on.exit(knitr::opts_hooks$restore(ohooks), add = TRUE)
    ##         templates <- knitr::opts_template$get()
    ##         on.exit(knitr::opts_template$restore(templates), add = TRUE)
    ##         if (pandoc2.0() && packageVersion("htmltools") >= "0.5.1") {
    ##             if (is.null(prev <- getOption("htmltools.preserve.raw"))) {
    ##                 options(htmltools.preserve.raw = TRUE)
    ##                 on.exit(options(htmltools.preserve.raw = prev), 
    ##                   add = TRUE)
    ##             }
    ##         }
    ##         if (is.function(output_format$on_exit)) 
    ##             on.exit(output_format$on_exit(), add = TRUE)
    ##         knitr::render_markdown()
    ##         knitr::opts_chunk$set(tidy = FALSE, error = FALSE)
    ##         if (!grepl("[.]html$", output_file)) 
    ##             knitr::opts_chunk$set(fig.retina = NULL)
    ##         knitr::opts_knit$set(rmarkdown.pandoc.from = output_format$pandoc$from, 
    ##             rmarkdown.pandoc.to = pandoc_to, rmarkdown.pandoc.args = output_format$pandoc$args, 
    ##             rmarkdown.pandoc.id_prefix = id_prefix, rmarkdown.keep_md = output_format$keep_md, 
    ##             rmarkdown.df_print = output_format$df_print, rmarkdown.version = 2, 
    ##             rmarkdown.runtime = runtime)
    ##         root_dir <- knit_root_dir
    ##         if (is.null(root_dir)) 
    ##             root_dir <- front_matter$knit_root_dir
    ##         if (!is.null(root_dir)) 
    ##             knitr::opts_knit$set(root.dir = root_dir)
    ##         base_pandoc_to <- gsub("[-+].*", "", pandoc_to)
    ##         if (base_pandoc_to == "html4") 
    ##             base_pandoc_to <- "html"
    ##         knitr::opts_chunk$set(fig.path = paste0(pandoc_path_arg(files_dir_slash, 
    ##             backslash = FALSE), "/figure-", base_pandoc_to, "/"))
    ##         cache_dir <- knitr_cache_dir(input, base_pandoc_to)
    ##         knitr::opts_chunk$set(cache.path = cache_dir)
    ##         cache_dir <- gsub("/$", "", cache_dir)
    ##         if (!is.null(output_format$knitr)) {
    ##             knitr::opts_knit$set(as.list(output_format$knitr$opts_knit))
    ##             knitr::opts_chunk$set(adjust_dev(as.list(output_format$knitr$opts_chunk)))
    ##             knitr::opts_template$set(as.list(output_format$knitr$opts_template))
    ##             knitr::knit_hooks$set(as.list(output_format$knitr$knit_hooks))
    ##             knitr::opts_hooks$set(as.list(output_format$knitr$opts_hooks))
    ##         }
    ##         knitr::opts_knit$set(rmarkdown.runtime = runtime)
    ##         if (is_shiny_prerendered(runtime)) {
    ##             shiny_prerendered_remove_uncached_data(original_input)
    ##             knitr::opts_hooks$set(label = shiny_prerendered_option_hook(original_input))
    ##             knitr::knit_hooks$set(evaluate = shiny_prerendered_evaluate_hook(original_input))
    ##         }
    ##         if (is_shiny_classic(runtime) && !is.null(shiny::getDefaultReactiveDomain())) {
    ##             knitr::knit_hooks$set(evaluate = function(code, envir, 
    ##                 ...) {
    ##                 if (identical(knitr::opts_current$get("label"), 
    ##                   "global")) {
    ##                   code_string <- one_string(code)
    ##                   if (!code_string %in% .globals$evaluated_global_chunks) {
    ##                     .globals$evaluated_global_chunks <- c(.globals$evaluated_global_chunks, 
    ##                       code_string)
    ##                     shiny::withReactiveDomain(NULL, {
    ##                       evaluate::evaluate(code, envir = globalenv(), 
    ##                         ...)
    ##                     })
    ##                   }
    ##                   else {
    ##                     list()
    ##                   }
    ##                 }
    ##                 else {
    ##                   evaluate::evaluate(code, envir, ...)
    ##                 }
    ##             })
    ##         }
    ##         if (!is.null(front_matter$params)) {
    ##             params <- knit_params_get(input_lines, params)
    ##             hasParams <- exists("params", envir = envir, inherits = FALSE)
    ##             envirParams <- NULL
    ##             if (hasParams) {
    ##                 envirParams <- get("params", envir = envir, inherits = FALSE)
    ##                 isKnownParamsObject <- inherits(envirParams, 
    ##                   "knit_param_list") || inherits(envirParams, 
    ##                   "knit_param")
    ##                 if (!isKnownParamsObject) {
    ##                   stop2("params object already exists in knit environment ", 
    ##                     "so can't be overwritten by render params")
    ##                 }
    ##             }
    ##             assign("params", params, envir = envir)
    ##             lockBinding("params", envir)
    ##             on.exit({
    ##                 if (exists("params", envir = envir, inherits = FALSE)) {
    ##                   do.call("unlockBinding", list("params", envir))
    ##                   if (hasParams) assign("params", envirParams, 
    ##                     envir = envir) else remove("params", envir = envir)
    ##                 }
    ##             }, add = TRUE)
    ##         }
    ##         sapply(as.list(getHook("rmarkdown.onKnit")), function(hook) {
    ##             tryCatch(hook(input = original_input), error = function(e) NULL)
    ##         })
    ##         on.exit({
    ##             sapply(as.list(getHook("rmarkdown.onKnitCompleted")), 
    ##                 function(hook) {
    ##                   tryCatch(hook(input = original_input), error = function(e) NULL)
    ##                 })
    ##         }, add = TRUE)
    ##         perf_timer_start("knitr")
    ##         input <- knitr::knit(knit_input, knit_output, envir = envir, 
    ##             quiet = quiet)
    ##         perf_timer_stop("knitr")
    ##         front_matter <- yaml_front_matter(input)
    ##         output_format$pandoc$args <- call_post_knit_handler()
    ##         rmd_warnings <- knit_meta_reset(class = "rmd_warning")
    ##         for (rmd_warning in rmd_warnings) {
    ##             message("Warning: ", rmd_warning)
    ##         }
    ##         shiny_prerendered_append_contexts(runtime, input)
    ##         knit_meta <- knit_meta_reset()
    ##     }
    ##     else {
    ##         output_format$pandoc$args <- call_post_knit_handler()
    ##     }
    ##     if (!(is_pandoc_to_html(output_format$pandoc) || identical(tolower(xfun::file_ext(output_file)), 
    ##         "html"))) {
    ##         if (has_html_dependencies(knit_meta)) {
    ##             if (!isTRUE(front_matter$always_allow_html)) {
    ##                 stop2("Functions that produce HTML output found in document targeting ", 
    ##                   pandoc_to, " output.\nPlease change the output type ", 
    ##                   "of this document to HTML. Alternatively, you can allow\n", 
    ##                   "HTML output in non-HTML formats by adding this option to the YAML front", 
    ##                   "-matter of\nyour rmarkdown file:\n\n", "  always_allow_html: true\n\n", 
    ##                   "Note however that the HTML output will not be visible in non-HTML formats.\n\n")
    ##             }
    ##         }
    ##         if (!identical(runtime, "static")) {
    ##             stop2("Runtime '", runtime, "' is not supported for ", 
    ##                 pandoc_to, " output.\nPlease change the output type ", 
    ##                 "of this document to HTML.")
    ##         }
    ##     }
    ##     intermediates_fig <- if (output_format$clean_supporting && 
    ##         !dir_exists(cache_dir)) {
    ##         fig_path <- gsub("/$", "", knitr::opts_chunk$get("fig.path"))
    ##         files_dir_fig <- list.files(files_dir, "^figure-.+")
    ##         if (length(files_dir_fig) < 1 || identical(files_dir_fig, 
    ##             basename(fig_path))) {
    ##             files_dir
    ##         }
    ##         else {
    ##             fig_path
    ##         }
    ##     }
    ##     intermediates <- c(intermediates, intermediates_fig)
    ##     if (run_pandoc) {
    ##         lua_env_vars <- xfun::set_envvar(c(RMARKDOWN_LUA_SHARED = pkg_file_lua("shared.lua")))
    ##         on.exit(xfun::set_envvar(lua_env_vars), add = TRUE)
    ##         perf_timer_start("pre-processor")
    ##         if (!is.null(output_format$pre_processor)) {
    ##             extra_args <- output_format$pre_processor(front_matter, 
    ##                 input, runtime, knit_meta, files_dir, output_dir)
    ##             output_format$pandoc$args <- c(output_format$pandoc$args, 
    ##                 extra_args)
    ##         }
    ##         if (is_shiny_prerendered(runtime)) {
    ##             shiny_prerendered_append_dependencies(input, shiny_prerendered_dependencies, 
    ##                 files_dir, output_dir)
    ##             output_format$pandoc$args <- c(output_format$pandoc$args, 
    ##                 pandoc_include_args(in_header = pkg_file("rmd/h/shiny-header.html")))
    ##         }
    ##         perf_timer_stop("pre-processor")
    ##         need_bibtex <- grepl("[.](pdf|tex)$", output_file) && 
    ##             any(c("--natbib", "--biblatex") %in% output_format$pandoc$args)
    ##         perf_timer_start("pandoc")
    ##         convert <- function(output, citeproc = FALSE) {
    ##             if (!is.null(intermediates_dir)) {
    ##                 figures_dir <- gsub("/$", "", knitr::opts_chunk$get("fig.path"))
    ##                 files <- list.files(figures_dir, full.names = TRUE, 
    ##                   recursive = TRUE)
    ##                 if (citeproc) 
    ##                   files <- c(files, front_matter[["bibliography"]])
    ##                 for (f in files) {
    ##                   intermediates <<- c(intermediates, copy_file_with_dir(f, 
    ##                     intermediates_dir))
    ##                 }
    ##             }
    ##             input <- path.expand(input)
    ##             output <- path.expand(output)
    ##             pandoc_args <- output_format$pandoc$args
    ##             if (!is.null(lua_filters <- output_format$pandoc$lua_filters)) {
    ##                 lua_filters <- pandoc_lua_filter_args(lua_filters)
    ##             }
    ##             pandoc_args <- c(lua_filters, pandoc_args)
    ##             input_files <- input
    ##             if (is.function(output_format$file_scope)) {
    ##                 input_files <- file_scope_split(input, output_format$file_scope)
    ##                 if (length(input_files) > 1) {
    ##                   pandoc_args <- c(pandoc_args, "--file-scope")
    ##                   on.exit(unlink(input_files), add = TRUE)
    ##                 }
    ##             }
    ##             if (!grepl(.shell_chars_regex, output) && !grepl(.shell_chars_regex, 
    ##                 input)) {
    ##                 return(pandoc_convert(input_files, pandoc_to, 
    ##                   output_format$pandoc$from, output, citeproc, 
    ##                   pandoc_args, !quiet))
    ##             }
    ##             ext <- xfun::file_ext(output)
    ##             if (ext != "") 
    ##                 ext <- paste0(".", ext)
    ##             pandoc_output_tmp <- basename(tempfile("pandoc", 
    ##                 getwd(), ext))
    ##             on.exit(unlink(pandoc_output_tmp), add = TRUE)
    ##             status <- pandoc_convert(input_files, pandoc_to, 
    ##                 output_format$pandoc$from, pandoc_output_tmp, 
    ##                 citeproc, pandoc_args, !quiet)
    ##             pandoc_output_tmp_path <- file.path(dirname(input), 
    ##                 pandoc_output_tmp)
    ##             renamed <- suppressWarnings(file.rename(pandoc_output_tmp_path, 
    ##                 output))
    ##             if (!renamed) {
    ##                 copied <- file.copy(pandoc_output_tmp_path, output, 
    ##                   overwrite = TRUE)
    ##                 if (!copied) {
    ##                   stop("failed to copy rendered pandoc artefact to '", 
    ##                     output, "'")
    ##                 }
    ##             }
    ##             status
    ##         }
    ##         texfile <- file_with_ext(output_file, "tex")
    ##         run_citeproc <- citeproc_required(front_matter, input_lines)
    ##         if (output_format$pandoc$keep_tex || pandoc_to %in% c("latex", 
    ##             "beamer")) {
    ##             convert(texfile, run_citeproc && !need_bibtex)
    ##             if (!("--template" %in% output_format$pandoc$args)) 
    ##                 patch_tex_output(texfile)
    ##             fix_horiz_rule(texfile)
    ##             if (!grepl("[.]tex$", output_file)) {
    ##                 latexmk(texfile, output_format$pandoc$latex_engine, 
    ##                   "--biblatex" %in% output_format$pandoc$args)
    ##                 file.rename(file_with_ext(texfile, "pdf"), output_file)
    ##                 if (!output_format$pandoc$keep_tex) {
    ##                   texfile <- normalize_path(texfile)
    ##                   on.exit(unlink(texfile), add = TRUE)
    ##                 }
    ##             }
    ##         }
    ##         else {
    ##             convert(output_file, run_citeproc)
    ##         }
    ##         if (!is.null(intermediates_dir)) {
    ##             intermediate_output <- file.path(intermediates_dir, 
    ##                 basename(output_file))
    ##             if (file.exists(intermediate_output)) {
    ##                 move_dir(intermediate_output, output_file)
    ##             }
    ##         }
    ##         perf_timer_stop("pandoc")
    ##         perf_timer_start("post-processor")
    ##         if (!is.null(output_format$post_processor)) 
    ##             output_file <- output_format$post_processor(front_matter, 
    ##                 input, output_file, clean, !quiet)
    ##         if (!quiet && getOption("rmarkdown.render.message", TRUE)) {
    ##             message("\nOutput created: ", relative_to(oldwd, 
    ##                 output_file))
    ##         }
    ##         perf_timer_stop("post-processor")
    ##     }
    ##     perf_timer_stop("render")
    ##     if (output_format$keep_md && !md_input) {
    ##         file.copy(input, file_with_ext(output_file, "md"), overwrite = TRUE)
    ##     }
    ##     if (run_pandoc) {
    ##         output_file <- abs_path(output_file)
    ##         if (length(output_meta <- output_metadata$get())) 
    ##             attr(output_file, "rmd_output_metadata") <- output_meta
    ##         invisible(output_file)
    ##     }
    ##     else {
    ##         intermediates <- setdiff(intermediates, c(input, intermediates_fig))
    ##         structure(input, knit_meta = knit_meta, files_dir = files_dir, 
    ##             intermediates_dir = intermediates_fig, intermediates = intermediates)
    ##     }
    ## }
    ## <bytecode: 0x00000246a9f5d4d0>
    ## <environment: namespace:rmarkdown>

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax
for authoring HTML, PDF, and MS Word documents. For more details on
using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that
includes both content as well as the output of any embedded R code
chunks within the document. You can embed an R code chunk like this:

``` r
summary(cars)
```

    ##      speed           dist       
    ##  Min.   : 4.0   Min.   :  2.00  
    ##  1st Qu.:12.0   1st Qu.: 26.00  
    ##  Median :15.0   Median : 36.00  
    ##  Mean   :15.4   Mean   : 42.98  
    ##  3rd Qu.:19.0   3rd Qu.: 56.00  
    ##  Max.   :25.0   Max.   :120.00

## Including Plots

You can also embed plots, for example:

![](testing-github-pages_files/figure-gfm/pressure-1.png)<!-- -->

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
