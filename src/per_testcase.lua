local output = function()
  return{
      header = function() end,
      footer = function() end,
      currently_executing = function(test_status, options) 
          return ""
        end,
      formatted_status = function(statuses, options, ms)
          io.write(";", ms, ";0;")
        end
    }
end

return output