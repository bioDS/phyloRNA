


gatk_mark_duplicates = function(input, output, nthreads=4, remake=FALSE){
    if(!remake && file.exists(output))
        return(invisible())

    log = paste0(output, ".txt")
    args = c(
        "MarkDuplicates",
        paste0("I=", input),
        paste0("O=", output),
        paste0("M=", log)
        )

    command = getOption("phyloRNA.gatk")
    systemE(command=command, args=args)
    }


gatk_sort = function(input, output, nthreads=4, remake=FALSE){
    if(!remake && file.exists(output))
        return(invisible())

    args = c(
        "SortSam",
        paste0("I=", input),
        paste0("O=", output),
        paste0("SORT_ORDER=", "coordinate")
        )
    command = getOption("phyloRNA.gatk")
    systemE(command=command, args=args)
    }
