package net.emaze.networks;

import junit.framework.Assert;
import org.junit.Test;

public class NetmaskTest {

    @Test
    public void narrowerYieldsANarrowerNetmask() {
        final Netmask narrower = Netmask.fromBits(16).narrower();
        Assert.assertEquals(Netmask.fromBits(17), narrower);
    }

    @Test(expected = IllegalStateException.class)
    public void cannotNarrowNarrowestNetmask() {
        Netmask.fromBits(32).narrower();
    }
    
    @Test
    public void isNarrowestYieldsTrueWhenNetmaskHas32bits() {
        Assert.assertTrue(Netmask.fromBits(32).isNarrowest());
    }
    
    @Test
    public void isNarrowestYieldsFalseWhenNetmaskHasLessThan32bits() {
        Assert.assertFalse(Netmask.fromBits(31).isNarrowest());
    }

    @Test
    public void widerYieldsAWiderNetmask() {
        final Netmask wider = Netmask.fromBits(16).wider();
        Assert.assertEquals(Netmask.fromBits(15), wider);
    }

    @Test(expected = IllegalStateException.class)
    public void cannotWidenWidestNetmask() {
        Netmask.fromBits(0).wider();
    }
    
    @Test
    public void isWidestYieldsTrueWhenNetmaskHas0bits(){
        Assert.assertTrue(Netmask.fromBits(0).isWidest());
    }

    @Test
    public void isWidestYieldsFalseWhenNetmaskHasMoreThan0bits(){
        Assert.assertFalse(Netmask.fromBits(1).isWidest());
    }

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
