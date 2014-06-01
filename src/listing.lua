local output = function()
  return{
      header = function() end,
      footer = function() end,
      currently_executing = function(test_status, options) 
          return ""
        end,
      formatted_status = function(statuses, options, ms)
          for k, v in pairs(statuses) do
            io.write(v['description'], '\n')
          end
        end
    }  
end

return output