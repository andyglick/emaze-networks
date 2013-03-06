package net.emaze.networks;

import junit.framework.Assert;
import org.junit.Test;

public class NetmaskTest {
    
    @Test
    public void netmaskFromSameLongAreEquals() {
        Assert.assertEquals(Netmask.fromBits(16), Netmask.fromBits(16));
    }
    
    @Test
    public void sameNetmaskInDifferentFormatsAreEquals() {
        Assert.assertEquals(Netmask.fromBits(16), Netmask.parse("255.255.0.0"));
    }
    
    @Test
    public void netmaskOfDifferentBitsAreDifferent() {
        Assert.assertFalse(Netmask.fromBits(16).equals(Netmask.fromBits(17)));
    }
    
    @Test
    public void netmaskIsDifferentFromNull() {
        Assert.assertFalse(Netmask.fromBits(16).equals(null));
    }
   
    @Test
    public void netmaskIsDifferentFromOtherObjects() {
        Assert.assertFalse(Netmask.fromBits(16).equals(new Object()));
    }
    

}
