pragma solidity ^0.4.21;

contract SimpleStorage {
    uint public count;

    event CountSet(uint _count);

    function setCount(uint _count) public {
        count = _count;
        emit CountSet(_count);
    }
}
