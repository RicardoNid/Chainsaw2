    
    %% 测试参数
   
    % 以比特/误比特数量作为停止依据
    maxErrNum = 1000;
    maxBitsNum = 1e6;

    % 以帧数量作为停止依据,运行100帧
    stopFrameNumber = 100 - 1;

    useIteration = 0;
    useEqualizer = 1;
    useInterleave = 1;
    useLdpc = 0;

    bitsPerSymbol = 4;
    % 可变参数包括N1, N2, bitAlloc和powAlloc
    % N1, N2需要取偶数
    N1 = 254; % 压缩前子载波数量
    N2 = 254; % 压缩后子载波数量
    bitAlloc = ones(N1, 1) * bitsPerSymbol;
    powAlloc = ones(N1, 1) * bitsPerSymbol;

    %% 固定帧尺寸参数
    fftSize = 512; 
    channelNum = fftSize / 2; % 采用HS-IFFT

    dataNum = 16; % 数据OFDM符号数量
    preambleNum = 2; % 训练OFDM符号数量
    frameNum = dataNum + preambleNum; % 总符号数量
    fullFrameBitsNum = dataNum * channelNum * bitsPerSymbol; % 最大数据比特数量
    
    %% 可调信道参数
    channelInUse = 3:N1+2; % 使用中的子载波
    paddingN1 = fftSize - (N1+2);

    issuedSize = N2*2; % 使用中的子载波
    channelIssued = 1:issuedSize; 
    paddingN2 = fftSize - N2*2; 
    
    % 信道参数确定后,相应的尺寸参数
    bitsPerOfdmSymbol = N1 * bitsPerSymbol;
    bitsPerFrame = dataNum * N1 * bitsPerSymbol;
    symbolsPerFrame = bitsPerFrame / bitsPerSymbol;
    guardBand = [0;0];
   
    %% QAM相关
    M = 2^bitsPerSymbol;
    
    %% FEC相关
    % 卷积编码参数
    constLen = 7;
    codeGen = [171,133];
    trellis = poly2trellis(constLen, codeGen);
    % LDPC编码参数
    P3_4 = [16 17 22 24  9  3 14 -1  4  2  7 -1 26 -1  2 -1 21 -1  1  0 -1 -1 -1 -1
     25 12 12  3  3 26  6 21 -1 15 22 -1 15 -1  4 -1 -1 16 -1  0  0 -1 -1 -1
     25 18 26 16 22 23  9 -1  0 -1  4 -1  4 -1  8 23 11 -1 -1 -1  0  0 -1 -1
      9  7  0  1 17 -1 -1  7  3 -1  3 23 -1 16 -1 -1 21 -1  0 -1 -1  0  0 -1
     24  5 26  7  1 -1 -1 15 24 15 -1  8 -1 13 -1 13 -1 11 -1 -1 -1 -1  0  0
      2  2 19 14 24  1 15 19 -1 21 -1  2 -1 24 -1  3 -1  2  1 -1 -1 -1 -1  0
    ]; % 802.11 3/4编码效率矩阵

    parallelFactor = 1/2;
    parallel = N1 * parallelFactor; % 卷积编码/维特比译码路数
    fecLength = 32 / parallelFactor; % 卷积编码分段长度

    % 为FEC进行padding
    if useLdpc
        ldpcIter = 5;

        % 面向最大可能尺寸进行LDPC编解码器设计
        actualBlockSize = ceil(fullFrameBitsNum / 24);
        codeLength = actualBlockSize * 24 ; % 编码器的输出长度,24为802.11中LDPC S-matrix的codeword尺寸基数
        paddingLength = codeLength - bitsPerFrame; % 输出长度-实际发射能力=padding数量

        codeRate = 3/4; % LDPC编码效率
       
        dataLength = codeLength * codeRate; % 编码器数据位数量
        checkLength = codeLength - dataLength; % 编码器校验位数量
        validLength = dataLength - paddingLength; % 受限于发射能力的数据位数量
        infoBitsPerFrame = validLength;
        
        % 下面给出LDPC编码器输入中的各个段落
        validRange = 1:validLength; 
        paddingRange = (dataLength - paddingLength + 1):dataLength;
        checkRange = dataLength+1:codeLength;
        % 输出中的有效(发射)部分
        issuedRange = [validRange, checkRange];
        assert(length(issuedRange) == bitsPerFrame, [num2str(length(issuedRange)), "!=" bitsPerFrame]);
    
        pcmatrix = ldpcQuasiCyclicMatrix(actualBlockSize,P3_4);

        actualCodeRate = validLength / bitsPerFrame;
        disp(['actual code rate = ', num2str(actualCodeRate)])
    else
        codeRate = 1/2;
        infoBitsPerFrame = bitsPerFrame * codeRate - parallel * (constLen - 1);
    end

    %% 
    cpLength = 20; % 循环前缀长度
    indiciesWithCP = [issuedSize-cpLength+1:issuedSize, 1:issuedSize]; % 用于添加CP的indicies
    indiciesWithoutCP = [cpLength+1:issuedSize+cpLength];  % 用于去掉CP的indicies
    
    % 交织相关    
    intrlvCol = 1;
    intrlvRow = parallel * 2; % 该参数下没有进行行列交织,仅进行循环移位
    interlvShift = 4;
    
    % 迭代译码相关
    iter = 4;

    % 信道响应和信道均衡
    seed = 42; % 
    rng(seed); % 通过固定的种子生成固定的preamble
    preambleLength = preambleNum * channelNum;
    preambleCol = randi([0,1], channelNum, 1) * 2 - 1; % 1和-1
    preamble = repmat(preambleCol, 1, preambleNum); % 训练符号
    preamble = preamble(channelInUse, :);
    
    movingAvgLength = 15;

    EbNo = 20; % 实际上是snr,但bertool匹配变量名为EbNo,因此使用EbNo/10
    noiseVariance = 2/10^(EbNo/10);
