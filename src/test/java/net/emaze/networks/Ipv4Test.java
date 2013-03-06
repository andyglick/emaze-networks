package net.emaze.networks;

import junit.framework.Assert;
import net.emaze.dysfunctional.order.Order;
import org.junit.Test;

public class Ipv4Test {
    
    private final static long LOCALHOST_AS_LONG = 2130706433L;
    
    @Test
    public void nextYieldsNextAddress() {
        final Ipv4 next = Ipv4.fromLong(LOCALHOST_AS_LONG).next();
        Assert.assertEquals(Ipv4.fromLong(LOCALHOST_AS_LONG + 1), next);
    }
    
    @Test(expected = IllegalStateException.class)
    public void nextAfterLastYieldsException() {
        Ipv4.LAST_IP.next();
    }
    
    @Test
    public void previousYieldsPreviousAddress() {
        final Ipv4 prev = Ipv4.fromLong(LOCALHOST_AS_LONG).previous();
        Assert.assertEquals(Ipv4.fromLong(LOCALHOST_AS_LONG - 1), prev);
    }
    
    @Test(expected = IllegalStateException.class)
    public void previousBeforeFirstYieldsException() {
        Ipv4.FIRST_IP.previous();
    }
    
    @Test
    public void offsetCanYieldAGreaterIp() {
        final Ipv4 displaced = Ipv4.fromLong(LOCALHOST_AS_LONG).offset(1);
        Assert.assertEquals(Ipv4.fromLong(LOCALHOST_AS_LONG + 1), displaced);
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void offsettingAfterLastIpYieldsException() {
        Ipv4.fromLong(LOCALHOST_AS_LONG).offset(Ipv4.LAST_IP.toLong());
    }
    
    @Test
    public void offsetCanYieldALesserIp() {
        final Ipv4 displaced = Ipv4.fromLong(LOCALHOST_AS_LONG).offset(-1);
        Assert.assertEquals(Ipv4.fromLong(LOCALHOST_AS_LONG - 1), displaced);
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void offsettingBeforeFirstIpYieldsException() {
        Ipv4.fromLong(LOCALHOST_AS_LONG).offset(-Ipv4.LAST_IP.toLong());
    }
    
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
    
    @Test
    public void ipv4FromSameLongsCompareAsEquals() {
        Assert.assertEquals(Order.EQ.order(), Ipv4.fromLong(LOCALHOST_AS_LONG).compareTo(Ipv4.fromLong(LOCALHOST_AS_LONG)));
    }
   
    @Test
    public void nextIpv4IsGreaterThan() {
        Assert.assertEquals(Order.GT.order(), Ipv4.fromLong(LOCALHOST_AS_LONG + 1).compareTo(Ipv4.fromLong(LOCALHOST_AS_LONG)));
    }
    
    @Test
    public void previousIpv4IsLesserThan() {
        Assert.assertEquals(Order.LT.order(), Ipv4.fromLong(LOCALHOST_AS_LONG - 1).compareTo(Ipv4.fromLong(LOCALHOST_AS_LONG)));
    }
    
    
   
}
