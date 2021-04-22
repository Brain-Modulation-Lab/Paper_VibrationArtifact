function coherence = get_coherence(x1,x2)
if size(x1,2) ~= size(x2,2)
  l=min(size(x1,2),size(x2,2));
  x1=x1(1,1:l);
  x2=x2(1,1:l);
end
x1 = x1 - mean(x1);
x2 = x2 - mean(x2);
coherence = [dot(hilbert(x2),x1),norm(x1),norm(x2)];
  
  