% features = [2     3     6     8    10    11    14    18    19    20    25    26    27    30    36    52    60    81   189   455   555];
features = 1:4999;

genre = load('../data/genredata.dat');

counts = spconvert(load('../data/wordcounts.dat'));
counts = counts(:,features);

fid = fopen('../data/words.dat');
words = textscan(fid,'%s','Delimiter','\n');
words = words{1}(features);
fclose(fid);
nWords = length(words);

tempGenre = genre;
predictions = zeros(size(genre));

for i = 1:length(genre)
  
  tempGenre(i) = -1;
  
  [prior, likelihood] = naiveBayesTrain(2, tempGenre, counts, 1);
  
  [predictedClass, predictedLogProbs] = naiveBayesTest(2, prior, likelihood, counts(i,:));
  
  predictions(i) = predictedClass;
  
  tempGenre(i) = genre(i);
  
end

confusionMatrix = zeros([2 2]);
for i = 1:2
  for j = 1:2
    confusionMatrix(i,j) = sum(predictions==i & genre==j);
  end
end


confusionMatrix

mean(predictions == genre)







%%
knnAccuracies = zeros([1 1000]);

doBinary = true;
doNormalize = true;

fprintf('\ndoBinary = %i, doNormalize = %i\n',doBinary,doNormalize);

for nfeatures = unique([1:100 2.^(0:12) 4999])
  
  tempGenre = genre;
  predictions = zeros(size(genre));
  
  for i = 1:length(genre)
    
    tempGenre(i) = -1;
    
    [prior, likelihood] = naiveBayesTrain(2, tempGenre, counts, 1);
    
    sortedLikelihoods = flipud(sortrows([abs(likelihood(1,:) - likelihood(2,:)); 1:nWords]'));
    
    features = sort(sortedLikelihoods(1:nfeatures,2))';
    
    % Extract features
    transformedCounts = counts(:, features);
    
    % Binarize
    if doBinary
      transformedCounts = transformedCounts > 0;
    end
    
    % Normalize
    if doNormalize
      transformedCounts = transformedCounts ./ repmat(sum(transformedCounts,2),[1 size(transformedCounts,2)]);
    end
  
    predictedClass = kNearestNeighbors(6, transformedCounts(i,:), 2, tempGenre, transformedCounts);
    
    predictions(i) = predictedClass;
    
    tempGenre(i) = genre(i);
    
  end
  
  knnAccuracies(nfeatures) = mean(predictions == genre);
  
  fprintf('With %i features, LOOCV accuracy = %4.2f%%\n', nfeatures, knnAccuracies(nfeatures) * 100);
end

figure(1)
validpoints = 1:4999; plot(validpoints(knnAccuracies~=0),knnAccuracies(knnAccuracies~=0))