a = [0,2+i,3+i,4+i];
c = [0,2+i,3-i,3+i];

aconj = flip(conj(a));
b = [a, 0, aconj(1:3)]

cconj = flip(conj(c));
d = [c, 0, cconj(1:3)]

ifft(b) - ifft(b, 'symmetric')
ifft(d) - ifft(d, 'symmetric')

ifft(b) - real(ifft(b+(d.*i)))
ifft(d) - imag(ifft(b+(d.*i)));

conj(a + (c.*i)) - (conj(c) + (conj(a).*i))