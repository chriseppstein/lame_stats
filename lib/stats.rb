module Stats
  class Counter
    include Enumerable
    
    def initialize(enumerable = nil)
      @hash = Hash.new
      @counts = 0
      add_all(enumerable) unless enumerable.nil?
    end
    def add_with_count(key,c)
      @hash[key] = (@hash[key] || 0) + c
      @counts += c
    end
    def add_all(enumerable)
      enumerable.each {|key| self << key }
    end
    def <<(key)
      add_with_count(key,1)
    end
    def size
      @hash.size
    end
    def [](key)
      @hash[key]
    end
    def each
      @hash.each_key { |k|
        yield k
      }
    end
    
    def delete_if
      @hash.delete_if do |k,v|
        returning(yield(k,v)) do |deleted|
          @counts -= v if deleted
        end
      end
    end
    
    def first
      [(k = @hash.keys.first),@hash[k]]
    end
    
    def each_count
      @hash.each_value { |v|
        yield v
      }
    end
    def each_with_count
      each { |k|
        yield k, self[k]
      }
    end
    def total
      return @counts
    end
    def max_count
      each_to_a(:each_count).max
    end
    def count_histogram
      return nil if @total == 0
      max = max_count
      returning(Histogram.new(max,max+1, 1)) do |histogram|
        each_count do |c|
          histogram << c
        end
      end
    end
    
    def each_t_value
      xbar = mean
      s = standard_deviation
      each_count do |c|
        yield compute_t_value(c, xbar, s)
      end
    end
    
    def compute_t_value(c, xbar = mean, s = standard_deviation)
      Math::sqrt(size)*(xbar - c)/s
    end
    
    def normal?(allowed_variance = 0.1)
      lr = LinearRegression.new
      x = -1
      each_t_value do |t|
        lr.add_point(x += 1, t)
      end
      puts "slope is #{lr.slope}"
      m = lr.slope
      m < allowed_variance && m > -allowed_variance
    end
    
    def sum_of_squares
      sos = 0
      each_count {|c| sos += c*c}
      return sos
    end
    
    def zscores(width = 3, center = 0, &block)
      silence_warnings do
        each_to_a(:each_with_zscore, width, center, &block)
      end
    end
    
    def mean
      total.to_f / size
    end
    
    def variance
      (sum_of_squares - total*mean)/(size)
    end
    
    def standard_deviation
      Math::sqrt(variance)
    end
    
    def each_with_zscore(width = 3, center = 0, &block)
      if size == 0
      elsif size == 1
        each {|k|
          yield k, center
        }
      else
        each_with_count { |k,v|
          zscore = (v - mean) / standard_deviation
          zscore = 0 if zscore.nan?
          block.call k, zscore*(width/3)+center
        }
      end
      nil
    end

    def poisson_lambda
      total/size
    end
  end
  
  class Histogram
    attr_accessor :bucket_count, :max_value, :min_value
    def initialize(bucket_count, max_value, min_value = 0, counts = nil)
      @bucket_count = bucket_count
      @min_value = min_value
      @max_value = max_value
      @counts = counts || [0] * bucket_count
    end
    
    def bucket_width
      @bucket_width ||= (@max_value - @min_value)/@bucket_count
    end
    
    def bucket_index_for(floor_bucket_value)
      ((floor_bucket_value - min_value).to_f/bucket_width).floor
    end
    
    def count_for(floor_bucket_value)
      @counts[bucket_index_for(floor_bucket_value)] || 0
    end
    
    def +(other)
      raise ArgumentError.new("incompatible histograms (#{self.bucket_width} != #{other.bucket_width})") unless self.bucket_width == other.bucket_width
      new_max = [max_value, other.max_value].max
      new_min = [min_value, other.min_value].min
      returning(Histogram.new((new_max-new_min)/bucket_width, new_max, new_min)) do |h|
        bv = new_min
        while bv < new_max
          h[h.bucket_index_for(bv)] = self.count_for(bv) + other.count_for(bv)
          bv += bucket_width
        end
      end
    end
    
    def bucket_index(value)
      (@bucket_count *(value - @min_value).to_f / (@max_value - @min_value)).floor
    end
    
    def <<(value)
      returning(bucket_index(value)) do |c|
        @counts[c] += 1
      end
    end
    
    def [](i)
      @counts[i]
    end
    
    def []=(i,v)
      @counts[i] = v
    end

    def to_s(options = {})
      if options[:orientation] == :vertical
        plot_vertically options
      else
        plot_horizontally options
      end
    end

    private

    def plot_vertically(options = {})
      scale = scale_to options[:max_height]
      height = (@counts.max * scale).ceil
      rows = (0..height).to_a.reverse.map do |h|
        @counts.map{|c| (c * scale).ceil >= h ? "*" : " " }.join('  ')
      end.join("\n") + "\n" + ("_" * ((@counts.size * 3) - 2))
    end

    def plot_horizontally(options = {})
      @counts.map {|c| "| "+"*"*(c*scale_to(options[:max_width])).ceil}.join("\n")
    end

    def scale_to(val)
      if val
        max = @counts.max
        max > val ? options[:max_width] / max.to_f : 1
      else
        1
      end
    end
  end
  
  class LinearRegression
    def initialize(points = [])
      @points = points
    end
    
    def add_point(x,y)
      @points << [x,y]
    end
    
    def sum(index)
      @points.inject(0){|s, p| s+p[index]}
    end
    
    def mean(index)
      sum(index) / @points.size
    end
    
    def sum_x;  sum(0);  end
    def mean_x; mean(0); end
    def sum_y;  sum(1);  end
    def mean_y; mean(1); end
    
    
    def sum_of_product_of_deviations
      my = mean_y
      mx = mean_x
      @points.inject(0) {|s,p| s + ((p[0] - mx) * (p[1] - my))}
    end
    
    def sum_of_squares_of_deviations(index = 0)
      m = mean(index)
      @points.inject(0){|s,p| s+((p[index] - m)**2)}
    end
    
    def slope
      sum_of_product_of_deviations / sum_of_squares_of_deviations
    end
    
    def y_intercept
      mean_y - mean_x * slope
    end
  end
end