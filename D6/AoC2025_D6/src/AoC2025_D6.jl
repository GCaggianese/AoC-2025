module AoC2025_D6

using DelimitedFiles
# Reads whitespace-delimited by default
data = readdlm("../input.txt")

# Extract operators (last line)
ops = data[end, :]

# Extract numbers
nums = data[1:end-1, :]

# Mathematics son
results = map(enumerate(ops)) do (i, op)
    col = nums[:, i]
    if op == "*"
        prod(col)  # Multiply all
    elseif op == "+"
        sum(col)   # Sum all
    else
        error("Unknown operation: $op")
    end
end

# Results as new row
data_with_results = vcat(data, results')

# Answer: sum of all results
answer = sum(results)

# println(data_with_results)

println(answer)

function parse_worksheet(filepath::String)
    lines = readlines(filepath)

    header_line = lines[end]
    num_lines = lines[1:end-1]

    max_width = maximum(length.(lines))

    header_padded = rpad(header_line, max_width)
    nums_padded = [rpad(line, max_width) for line in num_lines]

    # Expand header
    expanded_header = Char[]
    current_op = ' '
    for c in header_padded
        if c in ('*', '+')
            current_op = c
        end
        push!(expanded_header, current_op)
    end

    # Read vertically and group by separators
    current_group = []
    current_op = nothing
    results = []

    for col in 1:max_width
        digits = [nums_padded[row][col] for row in 1:length(nums_padded)]
        digit_str = join(filter(c -> c in '0':'9', digits))

        if isempty(digit_str)
            if !isempty(current_group)
                result = current_op == '*' ? prod(current_group) : sum(current_group)
                op_str = current_op == '*' ? " * " : " + "
                # println("$(join(current_group, op_str)) = $result")
                push!(results, result)
                current_group = []
            end
        else
            num = parse(Int, digit_str)
            push!(current_group, num)
            current_op = expanded_header[col]
        end
    end

    if !isempty(current_group)
        result = current_op == '*' ? prod(current_group) : sum(current_group)
        op_str = current_op == '*' ? " * " : " + "
        # println("$(join(current_group, op_str)) = $result")
        push!(results, result)
    end

    grand_total = sum(results)
    println("\nGrand total: $(join(results, " + ")) = $grand_total")

    return grand_total
end

parse_worksheet("../input.txt")

end # module AoC2025_D6
