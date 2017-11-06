contract SimpleStorage {
    
    uint public count;
    
    event NewCount(uint _count);
    
    function setCount(uint _count) {
        count = _count;
        NewCount(_count);
    }
}
