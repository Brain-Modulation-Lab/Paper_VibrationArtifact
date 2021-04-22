function coherence = get_coherence(x1,x2)
x1 = x1 - mean(x1);
x2 = x2 - mean(x2);
coherence = [dot(hilbert(x2),x1),norm(x1),norm(x2)];
  
  