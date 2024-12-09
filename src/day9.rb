class BlockEnumerator
    def initialize(input)
        @input = input
        @metablock = 0
        @position = 0
        @current_len = Integer(@input[@metablock])
        @position_sum = 0

        while @current_len == 0 do
            @metablock += 1
            if @metablock < @input.length then
                @current_len = Integer(@input[@metablock])
            end
        end
    end

    def has_next?()
        return @metablock < @input.length
    end

    def get()
        if @metablock % 2 == 0 then
            return @metablock / 2
        else
            return -1
        end
    end

    def next()
        @position += 1
        while @position >= @current_len do
            @position_sum += @position
            @position = 0
            @metablock += 1
            if @metablock < @input.length then
                @current_len = Integer(@input[@metablock])
            end
        end
    end

    def position()
        return @position_sum + @position
    end
end

class BackEnumerator
    def initialize(input)
        @n_blocks = input.length / 2
        @total_sum = (input.split("").map do |x| Integer(x) end).sum
        if input.length % 2 == 0 then
            @iter = BlockEnumerator.new((input + "0").reverse)
        else
            @iter = BlockEnumerator.new(input.reverse)
        end
    end

    def has_next?()
        while @iter.has_next? && @iter.get == -1 do
            @iter.next
        end
        return @iter.has_next?
    end

    def get()
        while @iter.has_next? && @iter.get == -1 do
            @iter.next
        end
        return @n_blocks - @iter.get()
    end

    def next()
        @iter.next
    end

    def position()
        while @iter.has_next? && @iter.get == -1 do
            @iter.next
        end
        return @total_sum - @iter.position
    end
end

input = File.open("./input/day9.txt").readline.strip

# Part 1:
fwd_enumerator = BlockEnumerator.new(input)
bck_enumerator = BackEnumerator.new(input)

checksum = 0
index = 0
debug = false

while
    fwd_enumerator.has_next? and
    bck_enumerator.has_next? and
    fwd_enumerator.position < bck_enumerator.position do
    if fwd_enumerator.get == -1 then
        current_block = bck_enumerator.get
        if debug then
            print bck_enumerator.get
        end
        bck_enumerator.next
        fwd_enumerator.next
    else
        current_block = fwd_enumerator.get
        if debug then
            print current_block
        end
        fwd_enumerator.next
    end
    checksum += index * current_block
    index += 1
end
if debug then
    print "\n"
end
puts checksum

# Part 2 (need to use a whole different method)
Span = Struct.new(:value, :length)
spans = []
input.split("").each_with_index do |c, index|
    if index % 2 == 0 then
        spans.push(Span.new(index / 2, Integer(c)))
    else
        spans.push(Span.new(-1, Integer(c)))
    end
end

current_block = input.length / 2

def merge_blanks(spans, around)
    raise "Assertion error: spans[around].value != -1" unless spans[around].value == -1
    if around < spans.length - 1 and spans[around + 1].value == -1 then
        spans[around].length += spans[around + 1].length
        spans.delete_at(around + 1)
    end
    if around > 0 and spans[around - 1].value == -1 then
        spans[around].length += spans[around - 1].length
        spans.delete_at(around - 1)
    end
end

# Oops, O(n^2). Could make it into `O(n)` with some effort but I don't want to :)
while current_block > 0 do
    index = spans.index do |span| span.value == current_block end
    current_span = spans[index]
    for candidate in 0..index do
        if spans[candidate].value == -1 and spans[candidate].length >= current_span.length then
            spans[candidate].length -= current_span.length
            spans[index] = Span.new(-1, current_span.length)
            spans.insert(candidate, current_span)
            merge_blanks(spans, index + 1)

            break
        end
    end
    current_block -= 1
end

index = 0
checksum = 0
spans.each do |span|
    for x in 1..span.length do
        if span.value != -1 then
            checksum += index * span.value
            if debug then
                print span.value
            end
        elsif debug then
            print "."
        end
        index += 1
    end
end
if debug then
    print "\n"
end
puts checksum
