# CVWithHaskell

UI tool for sending messages to Kafka topics written in Haskell
-- Installation stack & cabal

    https://docs.haskellstack.org/en/stable/README/
    https://www.haskell.org/cabal/download.html

-- Installation native deps:

    Needed for UI
    sudo apt-get install libgtk-3-0 libgtk-3-dev:

    Needed by Haskell build tools
    https://wiki.haskell.org/Gtk2Hs/Installation
    
-- Mac OS X
        
        https://docs.haskellstack.org/en/stable/README/
        curl -sSL https://get.haskellstack.org/ | sh
        
        sudo make install
        
        brew install gtk+3


-- Initial Build

    cd CVWithHaskell
    stack setup
    stack install gtk2hs-buildtools
    stack install c2hs
    stack build
    
-- Dev workflow    

     ./build.sh

-- Run

     stack run
or

     stack install ->  will install it in (~/.local/bin) you need it added  in $PATH

then

    CVWithHaskell - will run from anywhere (no need to be in project dir)

---
![My image](https://github.com/bemcho/KafkaConsumerProducerTool/blob/master/KafkaConsumerProducerTool.png)
