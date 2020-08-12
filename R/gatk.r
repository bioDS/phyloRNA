


gatk_mark_duplicates = function(input, output, remake=FALSE){
    if(!remake && file.exists(output))
        return(invisible())

    log = paste0(output, ".txt")
    args = c(
        "MarkDuplicates",
        "-I", input,
        "-O", output,
        "M", log
        )

    command = getOption("phyloRNA.gatk")
    systemE(command=command, args=args)
    }


gatk_sort = function(input, output, remake=FALSE){
    if(!remake && file.exists(output))
        return(invisible())

    args = c(
        "SortSam",
        "-I", input,
        "-O", output
        "-SORT_ORDER", "coordinate"
        )

    command = getOption("phyloRNA.gatk")
    systemE(command=command, args=args)
    }

