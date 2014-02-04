package net.emaze.networks.jackson;

import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import junit.framework.Assert;
import net.emaze.networks.jackson.IpMaskToStringTest.BeanWithNetmask;
import net.emaze.networks.Mask;
import org.junit.Test;

public class MaskFromStringTest {

    @Test
    public void deserializingYieldsExpected() throws IOException {
        final String serialized = "{'netmask':'255.255.255.0'}".replace("'", "\"");
        final Mask expected = Mask.netV4(24);
        final ObjectMapper mapper = new ObjectMapper();
        mapper.registerModule(new NetworksModule());
        final BeanWithNetmask got = mapper.readValue(serialized, BeanWithNetmask.class);
        Assert.assertEquals(expected, got.getNetmask());
    }

    @Test
    public void deserializingIPv6MaskYieldsExpected() throws IOException {
        final String serialized = "{'netmask':'115'}".replace("'", "\"");
        final Mask expected = Mask.netV6(115);
        final ObjectMapper mapper = new ObjectMapper();
        mapper.registerModule(new NetworksModule());
        final BeanWithNetmask got = mapper.readValue(serialized, BeanWithNetmask.class);
        Assert.assertEquals(expected, got.getNetmask());
    }
}
