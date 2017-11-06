contract SimpleStorage {
    
    uint count;
    
    event NewCount(uint _count);
    
    function setCount(uint _count) {
        count = _count;
        NewCount(_count);
    }
}
