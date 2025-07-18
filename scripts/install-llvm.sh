wget https://apt.llvm.org/llvm.sh 
chmod +x llvm.sh
sudo ./llvm.sh 15

# Install Polly and other LLVM development libraries
sudo apt-get update
sudo apt-get install -y \
    libpolly-15-dev \
    libomp-15-dev \
    llvm-15-dev \
    llvm-15-tools \
    libllvm15

cd /usr/bin/
sudo rm llvm-config
sudo ln -s llvm-config-15 llvm-config
cd /workspaces/caraml
rm llvm.sh