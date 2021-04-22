function coherence = plot_coherence_methods(x1,x2)
if size(x1,2) ~= size(x2,2)
  l=min(size(x1,2),size(x2,2));
  x1=x1(1,1:l);
  x2=x2(1,1:l);
end
x1 = x1 - mean(x1);
x2 = x2 - mean(x2);

figure()
subplot(2,1,1)
plot(x1)
hold on
plot(imag(hilbert(x1)))
subplot(2,1,2)
plot(x2)
hold on
plot([0,10],[-3000,-3000],'k')
saveas(gcf,'fig/DBS3011_S4_ecog_109_example_coherence_trace.pdf')

  