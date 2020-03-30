# CVWithHaskell
# KafkaConsumerProducerTool

-- Release v1.0

    https://github.com/bemcho/KafkaConsumerProducerTool/releases/tag/1.0
    
-- If your kafka is down or broken url is entered 

    for kafka broker Url the producer will hang for 5 min.
    as this is some request timeout i still can not configure
    

UI tool for sending messages to Kafka topics written in Haskell
-- Installation stack & cabal

    https://docs.haskellstack.org/en/stable/README/
    https://www.haskell.org/cabal/download.html

-- Installation native deps:

    Needed by hw-kafka haskell bindings:
    sudo apt-get install librdkafka++1 librdkafka-dev librdkafka1

    Needed for UI
    sudo apt-get install libgtk-3-0 libgtk-3-dev:

    Needed by Haskell build tools
    https://wiki.haskell.org/Gtk2Hs/Installation
    
-- Mac OS X
        
        https://docs.haskellstack.org/en/stable/README/
        curl -sSL https://get.haskellstack.org/ | sh
        
        git clone https://github.com/edenhill/librdkafka
        cd librdkafka
        ./configure.sh
        make
        sudo make install
        
        brew install gtk+3


-- Initial Build

    cd KafkaConsumerProducerTool
    stack setup
    stack install gtk2hs-buildtools
    stack install c2hs
    stack build
    
-- Dev workflow    

     stack solver --update-config
     ./build.sh

-- Run

     stack exec KafkaConsumerProducerTool
or

     stack install ->  will install it in (~/.local/bin) you need it added  in $PATH

then

    KafkaConsumerProducerTool - will run from anywhere (no need to be in project dir)

---
![My image](https://github.com/bemcho/KafkaConsumerProducerTool/blob/master/KafkaConsumerProducerTool.png)
