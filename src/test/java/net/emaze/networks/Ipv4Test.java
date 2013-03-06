package net.emaze.networks;

import junit.framework.Assert;
import org.junit.Test;

public class Ipv4Test {
    
    private final static long LOCALHOST_AS_LONG = 2130706433L;
    
    @Test
    public void ipv4FromSameLongAreEquals() {
        Assert.assertEquals(Ipv4.fromLong(LOCALHOST_AS_LONG), Ipv4.fromLong(LOCALHOST_AS_LONG));
    }
    
    @Test
    public void sameIpInDifferentFormatsAreEquals() {
        Assert.assertEquals(Ipv4.fromLong(LOCALHOST_AS_LONG), Ipv4.parse("127.0.0.1"));
    }
    
    @Test
    public void ipv4FromDifferentLongAreDifferent() {
        Assert.assertFalse(Ipv4.fromLong(LOCALHOST_AS_LONG).equals(Ipv4.fromLong(LOCALHOST_AS_LONG + 1)));
    }
    
    @Test
    public void ipv4IsDifferentFromNull() {
        Assert.assertFalse(Ipv4.fromLong(LOCALHOST_AS_LONG).equals(null));
    }
   
    @Test
    public void ipv4IsDifferentFromOtherObjects() {
        Assert.assertFalse(Ipv4.fromLong(LOCALHOST_AS_LONG).equals(new Object()));
    }
   
}
