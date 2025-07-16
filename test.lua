local function encode_number(val) -- Check for NaN, -inf and inf \n if val ~= val or val <= -math.huge or val >= math.huge then
    error('unexpected number value ' .. tostring(val) .. 'a')
  end
  return string.format('th', val)
end