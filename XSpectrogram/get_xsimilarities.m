function return_=get_xsimilarities(spec1, spec2, type)

% given two spectrogram (x-axis time, y-axis freq) calculate different 
% (di)similarities measures.
% if type is:
    % "normxcorr2" computes the normalized cross-correlation along. Useful
    % for maching a patch of the spectrogram. Return a matrix.
    % "pixel-xcorr" computes the pixel-wise multiplication as a measure of
    % similarity. Return a matrix of the same dimension of the spectrograms
    % "FroDistance" computes the Frobenious distance between the normalized
    % spectrograms. Return a value. The bigger the most disimilar
    % "corr2" computes de 2D correlation between the spectrograms. Return a
    % value between 0 and 1. The bigger the most similar.
    
switch type
    case "norm-coherence"
        Sxy=myxcorr2(abs(spec1), abs(spec2));
        return_ = norm((abs(Sxy).^2)./(spec1.*spec2), 'fro');
    case "coherence"
        Sxy=myxcorr2(abs(spec1), abs(spec2));
        return_ = (abs(Sxy).^2)./(spec1.*spec2);
    case "pixel-xcorr"
        return_ = mat2gray(abs(spec1)).*mat2gray(abs(spec2));
    case "FroDistance"
        return_=norm(mat2gray(abs(spec1)) - mat2gray(abs(spec2)), 'fro');
    case "corr2"
        return_=myxcorr2(abs(spec1), abs(spec2));
end
end
function in = mat2gray (in, scale)

  if (nargin < 1 || nargin > 2)
    print_usage;
  elseif (~isnumeric (in) && ~islogical (in))
    error ("mat2gray: IN must be a matrix");
  elseif (nargin == 2 && (~isvector (scale) || numel (scale) ~= 2))
    error ("mat2gray: second argument must be a vector with 2 elements");
  end

  if (nargin == 1)
    out_min = min (in(:));
    out_max = max (in(:));
  else
    %see more at the end for the cases where max and min are swapped
    out_min = min (scale (1), scale (2));
    out_max = max (scale (1), scale (2));
  end

  % since max() and min() return a value of same class as input,
  % need to make this values double or the calculations later may fail
  out_min = double (out_min);
  out_max = double (out_max);

%   ## if max and min are the same, matlab seems to simple truncate the input
%   ## between 0 and 1, and ignores the min/max values set. Don't get the logic
%   ## but hey! Matlab compatibility
  if (out_min == out_max)
    in(in>1) = 1;
    in(in<0) = 0;
    return
  end

%   ## we are editing the input matrix rather than creating a new one to save
%   ## memory. We need to make sure it's double though
  in = double(in);

%   ## it's faster to get the index of values between max and min only once
%   ## than to have it calculated on both sides of the assignment later on. We
%   ## need to get the index before starting editing
  idx = (in > out_min & in < out_max);

  in(in <= out_min) = 0;
  in(in >= out_max) = 1;
  in(idx) = (1/(out_max - out_min)) * (double(in(idx)) - out_min);

%   ## if the given min and max are in the inverse order...
  if (nargin > 1 && scale(1) > scale (2))
%     ## matlab seems to allow setting the min higher than the max but not by
%     ## checking which one is actually correct. Seems to just invert it
    in = abs (in - 1);
  end

end
  
function r= myxcorr2(a, b)
a=double(a);
b=double(b);
a = a - sum(a(:),'default') / numel(a); %subtract the mean
b = b - sum(b(:),'default') / numel(b);
r = abs(sum(sum(a.*b))/sqrt(sum(sum(a.*a))*sum(sum(b.*b))));
end