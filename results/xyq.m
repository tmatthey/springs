%
% read a matrix split into vctors - for plotting
%
fil=input('Enter input file:')
fid = fopen(fil);
x=[];
y=[];
z=[];
amat=[];
amat=fscanf(fid, '%g %g %g', [3,inf]);
fclose(fid);
n=size(amat,2);
for i = 1:n-10
  x(i) = amat(1,i+10);
  y(i) = amat(2,i+10);
  z(i) = amat(3,i+10);
end
mean(y);
std(y);
xlabel('timesteps');
ylabel('velocity [m/s]');
