function list = listfiles(basedir);
% This function should not exist but matlab is unable to consistently
%   return a char array of what is present in a damn given directory. (or I
%   don't know how to do this).
% 
a = dir(basedir);
for i = 1:(size(a,1)-2) % beurk franchement quoi
    list{i} = [a(i+2).name];
end
